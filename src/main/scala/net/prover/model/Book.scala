package net.prover.model

import java.io.File
import java.nio.file.{Files, Path, Paths}

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scala.collection.JavaConverters._

@JsonIgnoreProperties(Array("path", "dependencies", "existingTheoremCache", "context", "fullContext"))
case class Book(
    title: String,
    path: Path,
    dependencies: Seq[Book],
    chapters: Seq[Chapter]) {
  val key: String = title.formatAsKey

  def inferences: Seq[Inference] = chapters.flatMap(_.inferences)
  def inferenceTransforms: Seq[InferenceTransform] = chapters.flatMap(_.inferenceTransforms)
  def theorems: Seq[Theorem] = chapters.flatMap(_.theorems)

  def expandOutlines(theoremCache: Seq[Theorem]): Book = {
    val previousInferences = transitiveDependencies.flatMap(_.inferences)
    val previousInferenceTransforms = transitiveDependencies.flatMap(_.inferenceTransforms)
    copy(chapters = chapters.mapFold[Chapter] { case (chapter, expandedChapters) =>
        chapter.expandOutlines(
          previousInferences ++ expandedChapters.flatMap(_.inferences),
          previousInferenceTransforms ++ expandedChapters.flatMap(_.inferenceTransforms),
          theoremCache)
    })
  }

  protected def transitiveDependencies: Seq[Book] = Book.transitiveDependencies(dependencies)
}

object Book {
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

  private def importsParser: Parser[Seq[String]] = {
    Parser
      .optionalWord("import")
      .mapFlatMap(_ => Parser.toEndOfLine)
      .whileDefined
  }

  private def linesParser(
    bookTitle: String,
    bookPath: Path,
    context: ParsingContext
  ): Parser[(Seq[Chapter], ParsingContext)] = {
    Parser.iterateWhileDefined((Seq.empty[Chapter], context)) { case (chapters, currentContext) =>
      lineParser(bookTitle, bookPath, chapters, currentContext)
    }
  }

  private def lineParser(
    bookTitle: String,
    bookPath: Path,
    chapters: Seq[Chapter],
    context: ParsingContext
  ): Parser[Option[(Seq[Chapter], ParsingContext)]] = {
    Parser.singleWordIfAny.mapFlatMap {
      case "chapter" =>
        for {
          chapter <- chapterParser(bookTitle)
        } yield {
          (chapters :+ chapter, context)
        }
      case "variables" =>
        for {
          updatedContext <- variableDefinitionsParser(context)
        } yield {
          (chapters, updatedContext)
        }
      case "include" =>
        for {
          _ <- includeParser(bookPath, bookTitle)
        } yield {
          (chapters, context)
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
                  (previousChapters :+ updatedChapter, updatedContext)
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

  def includeParser(bookPath: Path, bookTitle: String): Parser[Unit] = Parser { tokenizer =>
    val (pathText, nextTokenizer) = Parser.toEndOfLine.parse(tokenizer)
    val includeTokenizer = Tokenizer.fromPath(bookPath.getParent.resolve(pathText)).copy(currentBook = Some(bookTitle))
    ((), nextTokenizer.addTokenizer(includeTokenizer))
  }

  private case class PreParsedBook(title: String, path: Path, imports: Seq[String], tokenizer: StringTokenizer)

  private def getPreParsedBooks(bookDirectoryPathName: String): Seq[PreParsedBook] = {
    FileUtils.listFiles(new File(bookDirectoryPathName), TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE)
      .asScala
      .map(_.toPath)
      .filter(_.getFileName.toString.endsWith(".book")).toSeq
      .map { path =>
        val tokenizer = Tokenizer.fromPath(path)
        if (tokenizer.isEmpty) throw new Exception(s"Book file '$path' was empty")

        val (title, tokenizerAfterTitle) = Parser.toEndOfLine.parse(tokenizer)
        val (imports, tokenizerAfterImports) = importsParser.parse(tokenizerAfterTitle)
        PreParsedBook(title, path, imports, tokenizerAfterImports.asInstanceOf[StringTokenizer].copy(currentBook = Some(title)))
      }
  }

  private def parseBook(
    book: PreParsedBook,
    previousBooks: Seq[Book],
    theoremCache: Seq[Theorem]
  ): Book = {
    val dependencies = book.imports.map { title =>
      previousBooks.find(_.title == title).getOrElse(throw new Exception(s"Could not find imported book '$title'"))
    }
    val transitiveDependencies = Book.transitiveDependencies(dependencies)
    val context = ParsingContext(
      transitiveDependencies.flatMap(_.chapters.flatMap(_.statementDefinitions)),
      transitiveDependencies.flatMap(_.chapters.flatMap(_.termDefinitions)),
      Set.empty,
      Set.empty)
    val chapters = linesParser(book.title, book.path, context)
      .parse(book.tokenizer)._1._1

    val b = Book(book.title, book.path, dependencies, chapters).expandOutlines(theoremCache)
    b
  }

  def fromDirectory(bookDirectoryPathName: String, theoremCache: Seq[Theorem]): Seq[Book] = {
    val preparsedBooks = getPreParsedBooks(bookDirectoryPathName)
    Files
      .readAllLines(Paths.get(bookDirectoryPathName, "books.list"))
      .asScala
      .foldLeft(Seq.empty[Book]) { case (previousBooks, bookTitle) =>
        val preParsedBook = preparsedBooks.find(_.title == bookTitle).getOrElse(throw new Exception(s"Could not find book '$bookTitle'"))
        previousBooks :+ parseBook(preParsedBook, previousBooks, theoremCache)
      }
  }
}
