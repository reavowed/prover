package net.prover.model

import java.io.File
import java.nio.file.Path

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scala.collection.JavaConverters._

@JsonIgnoreProperties(Array("path", "dependencies", "existingTheoremCache", "context", "fullContext"))
case class Book(
    title: String,
    path: Path,
    dependencies: Seq[Book],
    chapters: Seq[Chapter],
    context: Context) {
  val key: String = title.formatAsKey

  lazy val fullContext: Context = {
    context.combine(transitiveDependencies.map(_.context))
  }

  def theoremCache: Map[String, Theorem] = {
    chapters.flatMap(_.theoremCache).toMap
  }

  protected def transitiveDependencies: Seq[Book] = (dependencies.flatMap(_.transitiveDependencies) ++ dependencies).distinctBy(_.title)
}

object Book {
  val chapterEntryParsers: Seq[ChapterEntryParser[_]] = Seq(
    Comment,
    StatementDefinition,
    TermDefinition,
    Axiom,
    Theorem,
    InferenceTransform)
  val bookEntryParsers: Seq[BookEntryParser] = Seq(Chapter, BookInclude, VariableDefinitions) ++ chapterEntryParsers

  private def lineParser(book: Book): Parser[Option[Book]] = {
    Parser.singleWordIfAny.mapFlatMap { entryType =>
      bookEntryParsers.find(_.name == entryType)
        .getOrElse(throw new Exception(s"Unrecognised entry '$entryType'"))
        .parser(book)
    }
  }

  private def linesParser(book: Book): Parser[Book] = {
    Parser.iterateWhileDefined(book, lineParser)
  }

  private def importsParser: Parser[Seq[String]] = {
    Parser
      .singleWord.onlyIf(_ == "import")
      .mapFlatMap(_ => Parser.toEndOfLine)
      .whileDefined
  }

  private case class PreParsedBook(title: String, path: Path, imports: Seq[String], tokenizer: StringTokenizer)

  private def preparseBooks(bookFilePaths: Seq[Path]): Seq[PreParsedBook] = {
    bookFilePaths.map { path =>
      val tokenizer = Tokenizer.fromPath(path)
      if (tokenizer.isEmpty) throw new Exception(s"Book file '$path' was empty")

      val (title, tokenizerAfterTitle) = Parser.toEndOfLine.parse(tokenizer)
      val (imports, tokenizerAfterImports) = importsParser.parse(tokenizerAfterTitle)
      PreParsedBook(title, path, imports, tokenizerAfterImports.asInstanceOf[StringTokenizer].copy(currentBook = Some(title)))
    }
  }

  private def parseBook(
    book: PreParsedBook,
    dependentBooks: Seq[PreParsedBook],
    otherBooks: Seq[PreParsedBook],
    parsedBooks: Seq[Book],
    theoremCache: Map[String, Theorem]
  ): (Seq[Book], Seq[PreParsedBook]) = {
    book.imports.foldLeft[Either[Seq[Book], String]](Left(Nil)) {
      case (Left(books), title) =>
        parsedBooks.find(_.title == title).map(b => Left(books :+ b)).getOrElse(Right(title))
      case (r @ Right(_), _) =>
        r
    } match {
      case Left(dependencies) =>
        val initialBook = Book(book.title, book.path, dependencies, Nil, Context.empty.withTheoremCache(theoremCache))
        val parsedBook = linesParser(initialBook).parse(book.tokenizer)._1
        dependentBooks match {
          case dependentBook +: otherDependentBooks =>
            parseBook(dependentBook, otherDependentBooks, otherBooks, parsedBooks :+ parsedBook, theoremCache)
          case Nil =>
            (parsedBooks :+ parsedBook, otherBooks)
        }
      case Right(title) =>
        otherBooks.partition(_.title == title) match {
          case (Seq(dependency), otherOtherBooks) =>
            parseBook(dependency, book +: dependentBooks, otherOtherBooks, parsedBooks, theoremCache)
          case (Nil, _) =>
            dependentBooks.find(_.title == title) match {
              case Some(_) =>
                throw new Exception(s"Circular dependency detected in book '$title'")
              case None =>
                throw new Exception(s"Unrecognised book '$title'")
            }
        }
    }
  }

  private def parseBooks(books: Seq[PreParsedBook], theoremCache: Map[String, Theorem], parsedBooks: Seq[Book] = Nil): Seq[Book] = {
    books match {
      case Nil =>
        parsedBooks
      case book +: otherBooks =>
        val (moreParsedBooks, remainingUnparsedBooks) = parseBook(book, Nil, otherBooks, parsedBooks, theoremCache)
        parseBooks(remainingUnparsedBooks, theoremCache, moreParsedBooks)
    }
  }

  def fromDirectory(pathName: String, theoremCache: Map[String, Theorem]): Seq[Book] = {
    val bookFilePaths = FileUtils.listFiles(new File(pathName), TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE)
      .asScala
      .map(_.toPath)
      .filter(_.getFileName.toString.endsWith(".book"))
      .toSeq
    val preparsedBooks = preparseBooks(bookFilePaths)
    parseBooks(preparsedBooks, theoremCache)
  }
}
