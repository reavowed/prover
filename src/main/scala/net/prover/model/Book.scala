package net.prover.model

import java.nio.file.{Files, Path, Paths}
import java.time.Instant

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.util.Try

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
  def inferenceTransforms: Seq[InferenceTransform] = chapters.flatMap(_.inferenceTransforms)
  def theorems: Seq[Theorem] = chapters.flatMap(_.theorems)

  def expandOutlines(cachedProofs: Seq[CachedProof]): Book = {
    val previousInferences = transitiveDependencies.flatMap(_.inferences)
    val previousInferenceTransforms = transitiveDependencies.flatMap(_.inferenceTransforms)
    copy(chapters = chapters.mapFold[Chapter] { case (chapter, expandedChapters) =>
        chapter.expandOutlines(
          previousInferences ++ expandedChapters.flatMap(_.inferences),
          previousInferenceTransforms ++ expandedChapters.flatMap(_.inferenceTransforms),
          cachedProofs)
    })
  }

  def transitiveDependencies: Seq[Book] = Book.transitiveDependencies(dependencies)

  def cacheTheorems(cacheDirectoryPath: Path) = {
    for {
      chapter <- chapters
      theorem <- chapter.theorems
    } {
      val cachePath = cacheDirectoryPath.resolve(Paths.get(key, chapter.key, theorem.key))
      Files.createDirectories(cachePath.getParent)
      Files.write(cachePath, CachedProof(theorem.key, theorem.premises, theorem.proof).serialized.getBytes("UTF-8"))
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
    TheoremOutline,
    InferenceTransform)

  def parser(path: Path, cacheDirectoryPath: Path, availableDependencies: Seq[Book]): Parser[(Book, Map[Path, Instant])] = {
    for {
      title <- Parser.toEndOfLine
      imports <- importsParser
      dependencies = imports.map { importTitle =>
        availableDependencies.find(_.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$title'"))
      }
      transitiveDependencies = Book.transitiveDependencies(dependencies)
      context = ParsingContext(
        transitiveDependencies.flatMap(_.chapters.flatMap(_.statementDefinitions)),
        transitiveDependencies.flatMap(_.chapters.flatMap(_.termDefinitions)),
        transitiveDependencies.flatMap(_.statementVariableNames).toSet,
        transitiveDependencies.flatMap(_.termVariableNames).toSet)
      chaptersFileModificationTimesAndUpdatedParsingContext <- linesParser(title, path, context)
    } yield {
      val (chapters, fileModificationTimes, updatedParsingContext) = chaptersFileModificationTimesAndUpdatedParsingContext
      val bookCacheDirectoryPath = cacheDirectoryPath.resolve(title.formatAsKey)
      val cachedProofs = getAllFiles(bookCacheDirectoryPath).mapCollect { path =>
        val serializedProof = new String(Files.readAllBytes(path), "UTF-8")
        Try {
          CachedProof.parser(path.getFileName.toString)(updatedParsingContext).parseAndDiscard(serializedProof, path)
        }.ifFailed { e =>
          logger.info(s"Error parsing cached proof $path\n${e.getMessage}")
        }.toOption
      }
      val book = Book(
        title,
        path,
        dependencies,
        chapters,
        updatedParsingContext.statementVariableNames,
        updatedParsingContext.termVariableNames
      ).expandOutlines(cachedProofs)
      book.cacheTheorems(cacheDirectoryPath)
      (book, fileModificationTimes)
    }
  }

  def importsParser: Parser[Seq[String]] = {
    Parser
      .optionalWord("import")
      .mapFlatMap(_ => Parser.toEndOfLine)
      .whileDefined
  }

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
    Parser.singleWordIfAny.mapFlatMap {
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
