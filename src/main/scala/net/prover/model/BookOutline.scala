package net.prover.model

import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util

import net.prover.model.proof.{CachedProof, ProofEntries}
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

case class BookOutline(
  title: String,
  path: Path,
  dependencyOutlines: Seq[BookOutline],
  chapterOutlines: Seq[ChapterOutline],
  statementVariableNames: Seq[String],
  termVariableNames: Seq[String],
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
      Book(title, dependencies, chapters, statementVariableNames, termVariableNames)
    }.ifFailed { e =>
      BookOutline.logger.warn(s"Error expanding book '$title'\n${e.getMessage}")
    }.toOption
  }

  private def expandChapters(cachedProofs: Seq[CachedProof], dependencies: Seq[Book]): Seq[Chapter] = {
    val transitiveDependencies = dependencies.transitive
    val previousInferences = transitiveDependencies.flatMap(_.inferences)
    val previousStatementDefinitions = transitiveDependencies.flatMap(_.statementDefinitions)
    chapterOutlines.mapFold[Chapter] { case (expandedChapters, chapterOutline) =>
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

  def parse(title: String, path: Path, availableDependencies: Seq[BookOutline], getChapterPath: (String, Int) => Path): Option[BookOutline] = {
    val key = title.formatAsKey
    Try(parser(path, availableDependencies, getChapterPath).parse(Tokenizer.fromPath(path))._1) match {
      case Success(bookOutline) =>
        Some(bookOutline)
      case Failure(e) =>
        logger.error(s"Error parsing book '$title'\n${e.getMessage}")
        None
    }
  }

  def parser(path: Path, availableDependencies: Seq[BookOutline], getChapterPath: (String, Int) => Path): Parser[BookOutline] = {
    for {
      title <- Parser.toEndOfLine
      imports <- importsParser
      dependencies = imports.map { importTitle =>
        availableDependencies.find(_.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$importTitle'"))
      }
      transitiveDependencies = dependencies.transitive
      statementAndTermVariableNames <- variableDefinitionsParser
      chapterTitles <- chapterTitlesParser
    } yield {
      val (statementVariableNames, termVariableNames) = statementAndTermVariableNames
      val initialContext = ParsingContext(
        transitiveDependencies.flatMap(_.chapterOutlines.flatMap(_.statementDefinitions)),
        transitiveDependencies.flatMap(_.chapterOutlines.flatMap(_.termDefinitions)),
        statementVariableNames.toSet,
        termVariableNames.toSet,
        Seq.empty)
      val (updatedParsingContext, chapterOutlines) = chapterTitles.zipWithIndex.mapFold(initialContext) { case (context, (chapterTitle, index)) =>
          val chapterPath = getChapterPath(chapterTitle, index)
          val (chapterOutline, newContext) = ChapterOutline.parser(chapterTitle, title)(context).parseAndDiscard(chapterPath)
          (newContext, chapterOutline)
        }
      BookOutline(
          title,
          path,
          dependencies,
          chapterOutlines,
          statementVariableNames,
          termVariableNames,
          updatedParsingContext)
    }
  }

  private def importsParser: Parser[Seq[String]] = {
    Parser
      .optionalWord("import")
      .flatMapMap(_ => Parser.toEndOfLine)
      .whileDefined
  }

  def variableDefinitionsParser: Parser[(Seq[String], Seq[String])] = {
    Parser.optionalWord("variables").flatMapMap { _ =>
      for {
        statementVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
        termVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
      } yield {
        (statementVariableNames, termVariableNames)
      }
    }.getOrElse((Nil, Nil))
  }

  def chapterTitlesParser: Parser[Seq[String]] = {
    Parser.optionalWord("chapter").flatMapMap(_ => Parser.toEndOfLine).whileDefined
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