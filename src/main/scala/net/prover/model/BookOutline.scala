package net.prover.model

import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util

import net.prover.model.entries._
import net.prover.model.proof.{CachedProof, ProofEntries}
import org.slf4j.LoggerFactory

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

case class BookOutline(
  title: String,
  path: Path,
  dependencyOutlines: Seq[BookOutline],
  chapterOutlines: Seq[ChapterOutline],
  statementVariableNames: Set[String],
  termVariableNames: Set[String],
  parsingContext: ParsingContext)
{
  val key: String = title.formatAsKey

  def proveTheorems(cacheDirectoryPath: Path, previousBooks: Seq[Book]): Option[Book] = {
    Try {
      val bookCacheDirectoryPath = cacheDirectoryPath.resolve(title.formatAsKey)
      val dependencies = dependencyOutlines.map { dependencyOutline =>
        previousBooks.find(_.title == dependencyOutline.title)
          .getOrElse(throw new Exception(s"Could not find imported book '${dependencyOutline.title}'"))
      }
      val cachedProofs = bookCacheDirectoryPath.getAllChildFiles.mapCollect { path =>
        val serializedProof = new String(Files.readAllBytes(path), "UTF-8")
        Try {
          CachedProof.parser(path)(parsingContext).parseAndDiscard(serializedProof, path)
        }.ifFailed { e =>
          BookOutline.logger.info(s"Error parsing cached proof $path\n${e.getMessage}")
        }.toOption
      }
      val chapters = expandChapters(cachedProofs, dependencies)
      cacheTheorems(chapters, cacheDirectoryPath)
      Book(title, dependencies, chapters)
    }.ifFailed { e =>
      BookOutline.logger.warn(s"Error expanding book '$title'\n${e.getMessage}")
    }.toOption
  }

  private def expandChapters(cachedProofs: Seq[CachedProof], dependencies: Seq[Book]): Seq[Chapter] = {
    val transitiveDependencies = dependencies.transitive
    val previousInferences = transitiveDependencies.flatMap(_.inferences)
    val previousStatementDefinitions = transitiveDependencies.flatMap(_.statementDefinitions)
    chapterOutlines.mapFold[Chapter] { case (chapterOutline, expandedChapters) =>
      chapterOutline.expand(
        ProofEntries(
          previousInferences ++ expandedChapters.flatMap(_.inferences),
          previousStatementDefinitions ++ expandedChapters.flatMap(_.statementDefinitions)),
        cachedProofs)
    }
  }

  private def cacheTheorems(chapters: Seq[Chapter], cacheDirectoryPath: Path): Unit = {
    val bookCachePath = cacheDirectoryPath.resolve(key)
    val cachedProofs = for {
      chapter <- chapters
      theorem <- chapter.theorems
    } yield {
      val cachePath = bookCachePath.resolve(Paths.get(chapter.key, theorem.key))
      CachedProof(cachePath, theorem.premises, theorem.proof.steps.map(_.cached))
    }
    val existingFiles = bookCachePath.getAllChildFiles
    val (filesAdded, filesUpdated, filesToRemove) = cachedProofs
      .foldLeft((0, 0, existingFiles)) { case ((add, update, remaining), cachedProof) =>
        if (remaining.contains(cachedProof.path)) {
          val currentContent = Files.readAllBytes(cachedProof.path)
          val newContent = cachedProof.serialized.getBytes("UTF-8")
          if (util.Arrays.equals(currentContent, newContent))
            (add, update, remaining.filter(_ != cachedProof.path))
          else {
            Files.createDirectories(cachedProof.path.getParent)
            Files.write(cachedProof.path, newContent)
            (add, update + 1, remaining.filter(_ != cachedProof.path))
          }
        } else {
          Files.createDirectories(cachedProof.path.getParent)
          Files.write(cachedProof.path, cachedProof.serialized.getBytes("UTF-8"))
          (add + 1, update, remaining)
        }
      }
    filesToRemove.foreach(Files.delete)
    if (filesAdded != 0 || filesUpdated != 0 || filesToRemove.nonEmpty) {
      BookOutline.logger.info(s"Updated cache ${bookCachePath.toString}: " +
        s"$filesAdded added, $filesUpdated updated, ${filesToRemove.size} removed")
    }
  }
}

object BookOutline {
  val logger = LoggerFactory.getLogger(BookOutline.getClass)

  val chapterEntryParsers: Seq[ChapterEntryParser[_]] = Seq(
    Comment,
    StatementDefinition,
    TermDefinition,
    AxiomOutline,
    TheoremOutline,
    Shorthand)

  def parse(title: String, bookDirectoryPath: Path, availableDependencies: Seq[BookOutline]): (Option[BookOutline], Map[Path, Instant]) = {
    val key = title.formatAsKey
    val path = bookDirectoryPath.resolve(key).resolve(key + ".book")
    val bookModificationTime = Files.getLastModifiedTime(path).toInstant
    Try(parser(path, availableDependencies).parse(Tokenizer.fromPath(path))._1) match {
      case Success((bookOutline, subModificationTimes)) =>
        (Some(bookOutline), subModificationTimes.updated(path, bookModificationTime))
      case Failure(ExceptionWithModificationTimes(e, subModificationTimes)) =>
        logger.error(s"Error parsing book '$title'\n${e.getMessage}")
        (None, subModificationTimes.updated(path, bookModificationTime))
      case Failure(e) =>
        logger.error(s"Error parsing book '$title'\n${e.getMessage}")
        (None, Map(path -> bookModificationTime))
    }
  }

  def parser(path: Path, availableDependencies: Seq[BookOutline]): Parser[(BookOutline, Map[Path, Instant])] = {
    for {
      title <- Parser.toEndOfLine
      imports <- importsParser
      dependencies = imports.map { importTitle =>
        availableDependencies.find(_.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$importTitle'"))
      }
      transitiveDependencies = dependencies.transitive
      context = ParsingContext(
        transitiveDependencies.flatMap(_.chapterOutlines.flatMap(_.statementDefinitions)),
        transitiveDependencies.flatMap(_.chapterOutlines.flatMap(_.termDefinitions)),
        Set.empty,
        Set.empty,
        Seq.empty)
      chapterOutlinesFileModificationTimesAndUpdatedParsingContext <- linesParser(title, path, context)
    } yield {
      val (chapterOutlines, fileModificationTimes, updatedParsingContext) = chapterOutlinesFileModificationTimesAndUpdatedParsingContext
      (BookOutline(
          title,
          path,
          dependencies,
          chapterOutlines,
          updatedParsingContext.statementVariableNames,
          updatedParsingContext.termVariableNames,
          updatedParsingContext),
        fileModificationTimes)
    }
  }

  private def importsParser: Parser[Seq[String]] = {
    Parser
      .optionalWord("import")
      .flatMapMap(_ => Parser.toEndOfLine)
      .whileDefined
  }

  private def linesParser(
    bookTitle: String,
    bookPath: Path,
    context: ParsingContext
  ): Parser[(Seq[ChapterOutline], Map[Path, Instant], ParsingContext)] = {
    Parser.iterateWhileDefined(
      (Seq.empty[ChapterOutline], Map.empty[Path, Instant], context)
    ) { case (chapterOutlines, fileModificationTimes, currentContext) =>
      lineParser(bookTitle, bookPath, chapterOutlines, fileModificationTimes, currentContext)
    }
  }

  private def lineParser(
    bookTitle: String,
    bookPath: Path,
    chapterOutlines: Seq[ChapterOutline],
    fileModificationTimes: Map[Path, Instant],
    context: ParsingContext
  ): Parser[Option[(Seq[ChapterOutline], Map[Path, Instant], ParsingContext)]] = {
    val unsafeParser = Parser.singleWordIfAny.flatMapMap {
      case "chapter" =>
        for {
          chapterOutline <- chapterOutlineParser(bookTitle)
        } yield {
          (chapterOutlines :+ chapterOutline, fileModificationTimes, context)
        }
      case "variables" =>
        for {
          updatedContext <- variableDefinitionsParser(context)
        } yield {
          (chapterOutlines, fileModificationTimes, updatedContext)
        }
      case "include" =>
        for {
          newModificationTime <- includeParser(bookPath, bookTitle)
        } yield {
          (chapterOutlines, fileModificationTimes + newModificationTime, context)
        }
      case key =>
        chapterEntryParsers.find(_.name == key) match {
          case Some(chapterEntryParser) =>
            chapterOutlines match {
              case previousChapters :+ currentChapter =>
                for {
                  chaptersAndContext <- chapterEntryParser.parseToChapterOutline(currentChapter, context)
                  updatedChapter = chaptersAndContext._1
                  updatedContext = chaptersAndContext._2
                } yield {
                  (previousChapters :+ updatedChapter, fileModificationTimes, updatedContext)
                }
              case Nil =>
                throw new Exception(s"Cannot parse chapter entry '$key' outside of a chapter")
            }
          case None =>
            throw new Exception(s"Unrecognised entry $key")
        }
    }
    Parser { tokenizer =>
      try {
        unsafeParser.parse(tokenizer)
      } catch {
        case NonFatal(e) =>
          throw ExceptionWithModificationTimes(e, fileModificationTimes)
      }
    }
  }

  def chapterOutlineParser(bookTitle: String): Parser[ChapterOutline] = {
    for {
      title <- Parser.toEndOfLine
      summary <- Parser.toEndOfLine
    } yield {
      ChapterOutline(title, summary, bookTitle)
    }
  }

  def variableDefinitionsParser(context: ParsingContext): Parser[ParsingContext] = {
    for {
      newStatementVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
      newTermVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
    } yield {
      context.copy(
        statementVariableNames = context.statementVariableNames ++ newStatementVariableNames,
        termVariableNames = context.termVariableNames ++ newTermVariableNames)
    }
  }

  def includeParser(bookPath: Path, bookTitle: String): Parser[(Path, Instant)] = Parser { tokenizer =>
    val (pathText, nextTokenizer) = Parser.toEndOfLine.parse(tokenizer)
    val includedPath = bookPath.getParent.resolve(pathText)
    if (!Files.exists(includedPath)) {
      throw new Exception(s"Included file $includedPath does not exist")
    }
    val modificationTime = Files.getLastModifiedTime(includedPath).toInstant
    val includeTokenizer = Tokenizer.fromPath(includedPath).copy(currentBook = Some(bookTitle))
    ((includedPath, modificationTime), nextTokenizer.addTokenizer(includeTokenizer))
  }

  implicit class BookOutlineSeqOps(bookOutlines: Seq[BookOutline]) {
    def transitive: Seq[BookOutline] = {
      (bookOutlines.flatMap(_.dependencyOutlines.transitive) ++ bookOutlines).distinctBy(_.title)
    }
  }

  case class ExceptionWithModificationTimes(cause: Throwable, modificationTimes: Map[Path, Instant])
    extends Exception(cause)
      with ParseException.NoWrap
}