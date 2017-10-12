package net.prover.model

import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries._
import net.prover.model.proof.CachedProof
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

@JsonIgnoreProperties(Array("path", "dependencies", "existingTheoremCache", "context", "fullContext"))
case class Book(
    title: String,
    path: Path,
    dependencies: Seq[Book],
    chapters: Seq[Chapter],
    statementVariableNames: Set[String],
    termVariableNames: Set[String]) {
  val key: String = title.formatAsKey

  def inferences: Seq[Inference] = chapters.flatMap(_.inferences)
  def theorems: Seq[Theorem] = chapters.flatMap(_.theorems)
  def transformations: Seq[StatementDefinition] = chapters.flatMap(_.transformations)

  def expandOutlines(cachedProofs: Seq[CachedProof]): Book = {
    val previousInferences = transitiveDependencies.flatMap(_.inferences)
    val previousTransformations = transitiveDependencies.flatMap(_.transformations)
    copy(chapters = chapters.mapFold[Chapter] { case (chapter, expandedChapters) =>
        chapter.expandOutlines(
          previousInferences ++ expandedChapters.flatMap(_.inferences),
          previousTransformations ++ expandedChapters.flatMap(_.transformations),
          cachedProofs)
    })
  }

  def transitiveDependencies: Seq[Book] = Book.transitiveDependencies(dependencies)

  def cacheTheorems(cacheDirectoryPath: Path) = {
    val bookCachePath = cacheDirectoryPath.resolve(key)
    val cachedProofs = for {
      chapter <- chapters
      theorem <- chapter.theorems
    } yield {
      val cachePath = bookCachePath.resolve(Paths.get(chapter.key, theorem.key))
      CachedProof(cachePath, theorem.premises, theorem.proof.steps.map(_.cached))
    }
    val existingFiles = Book.getAllFiles(bookCachePath)
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
      Book.logger.info(s"Updated cache ${bookCachePath.toString}: " +
        s"$filesAdded added, $filesUpdated updated, ${filesToRemove.size} removed")
    }
  }
}

object Book {
  val logger = LoggerFactory.getLogger(Book.getClass)

  def transitiveDependencies(books: Seq[Book]): Seq[Book] = {
    (books.flatMap(_.transitiveDependencies) ++ books).distinctBy(_.title)
  }

  val chapterEntryParsers: Seq[ChapterEntryParser[_]] = Seq(
    Comment,
    StatementDefinition,
    TermDefinition,
    AxiomOutline,
    TheoremOutline)

  def parseBook(title: String, bookDirectoryPath: Path, cacheDirectoryPath: Path, availableDependencies: Seq[Book]): (Option[Book], Map[Path, Instant]) = {
    val key = title.formatAsKey
    val path = bookDirectoryPath.resolve(key).resolve(key + ".book")
    val bookModificationTime = Files.getLastModifiedTime(path).toInstant
    val parser = Book.parser(path, cacheDirectoryPath, availableDependencies)
    val (outlineBookAndContextOption, modificationTimes) = Try(parser.parse(Tokenizer.fromPath(path))._1) match {
      case Success((outlineBook, context, subModificationTimes)) =>
        (Some((outlineBook, context)), subModificationTimes.updated(path, bookModificationTime))
      case Failure(ExceptionWithModificationTimes(e, subModificationTimes)) =>
        logger.error(s"Error parsing book '$title'\n${e.getMessage}")
        (None, subModificationTimes.updated(path, bookModificationTime))
      case Failure(e) =>
        logger.error(s"Error parsing book '$title'\n${e.getMessage}")
        (None, Map(path -> bookModificationTime))
    }
    val bookOption = outlineBookAndContextOption.flatMap { case (bookOutline, context) =>
      Try {
        val bookCacheDirectoryPath = cacheDirectoryPath.resolve(title.formatAsKey)
        val cachedProofs = getAllFiles(bookCacheDirectoryPath).mapCollect { path =>
          val serializedProof = new String(Files.readAllBytes(path), "UTF-8")
          Try {
            CachedProof.parser(path)(context).parseAndDiscard(serializedProof, path)
          }.ifFailed { e =>
            logger.info(s"Error parsing cached proof $path", e)
          }.toOption
        }
        val book = bookOutline.expandOutlines(cachedProofs)
        book.cacheTheorems(cacheDirectoryPath)
        book
      }.ifFailed { e =>
        logger.warn(s"Error expanding book '${bookOutline.title}'\n${e.getMessage}")
      }.toOption
    }
    (bookOption, modificationTimes)
  }

  def parser(path: Path, cacheDirectoryPath: Path, availableDependencies: Seq[Book]): Parser[(Book, ParsingContext, Map[Path, Instant])] = {
    for {
      title <- Parser.toEndOfLine
      imports <- importsParser
      dependencies = imports.map { importTitle =>
        availableDependencies.find(_.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$importTitle'"))
      }
      transitiveDependencies = Book.transitiveDependencies(dependencies)
      context = ParsingContext(
        transitiveDependencies.flatMap(_.chapters.flatMap(_.statementDefinitions)),
        transitiveDependencies.flatMap(_.chapters.flatMap(_.termDefinitions)),
        Set.empty,
        Set.empty,
        Seq.empty)
      chaptersFileModificationTimesAndUpdatedParsingContext <- linesParser(title, path, context)
    } yield {
      val (chapters, fileModificationTimes, updatedParsingContext) = chaptersFileModificationTimesAndUpdatedParsingContext
      (
        Book(
          title,
          path,
          dependencies,
          chapters,
          updatedParsingContext.statementVariableNames,
          updatedParsingContext.termVariableNames),
        updatedParsingContext,
        fileModificationTimes)
    }
  }

  def importsParser: Parser[Seq[String]] = {
    Parser
      .optionalWord("import")
      .flatMapMap(_ => Parser.toEndOfLine)
      .whileDefined
  }

  case class ExceptionWithModificationTimes(cause: Throwable, modificationTimes: Map[Path, Instant])
    extends Exception(cause)
    with ParseException.NoWrap

  private def linesParser(
    bookTitle: String,
    bookPath: Path,
    context: ParsingContext
  ): Parser[(Seq[Chapter], Map[Path, Instant], ParsingContext)] = {
    Parser.iterateWhileDefined((Seq.empty[Chapter], Map.empty[Path, Instant], context)) { case (chapters, fileModificationTimes, currentContext) =>
      lineParser(bookTitle, bookPath, chapters, fileModificationTimes, currentContext)
    }
  }

  private def lineParser(
    bookTitle: String,
    bookPath: Path,
    chapters: Seq[Chapter],
    fileModificationTimes: Map[Path, Instant],
    context: ParsingContext
  ): Parser[Option[(Seq[Chapter], Map[Path, Instant], ParsingContext)]] = {
    val unsafeParser = Parser.singleWordIfAny.flatMapMap {
      case "chapter" =>
        for {
          chapter <- chapterParser(bookTitle)
        } yield {
          (chapters :+ chapter, fileModificationTimes, context)
        }
      case "variables" =>
        for {
          updatedContext <- variableDefinitionsParser(context)
        } yield {
          (chapters, fileModificationTimes, updatedContext)
        }
      case "include" =>
        for {
          newModificationTime <- includeParser(bookPath, bookTitle)
        } yield {
          (chapters, fileModificationTimes + newModificationTime, context)
        }
      case key =>
        chapterEntryParsers.find(_.name == key) match {
          case Some(chapterEntryParser) =>
            chapters match {
              case previousChapters :+ currentChapter =>
                for {
                  chaptersAndContext <- chapterEntryParser.parseToChapter(currentChapter, context)
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

  def chapterParser(bookTitle: String): Parser[Chapter] = {
    for {
      title <- Parser.toEndOfLine
      summary <- Parser.toEndOfLine
    } yield {
      Chapter(title, summary, bookTitle)
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

  private def getAllFiles(directoryPath: Path): Seq[Path] = {
    if (directoryPath.toFile.isDirectory)
      FileUtils.listFiles(directoryPath.toFile, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE).asScala
        .map(_.toPath)
        .toSeq
    else
      Nil
  }
}
