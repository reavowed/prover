package net.prover.model

import java.io.File
import java.nio.file.Path

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scala.collection.JavaConverters._

@JsonIgnoreProperties(Array("path", "dependencies", "context", "fullContext"))
case class Book(
    title: String,
    path: Path,
    dependencies: Seq[Book],
    chapters: Seq[Chapter] = Nil,
    context: Context = Context.empty) {
  val key: String = title.formatAsKey

  lazy val fullContext: Context = {
    context.combine(transitiveDependencies.map(_.context))
  }

  protected def transitiveDependencies: Seq[Book] = dependencies.flatMap(_.transitiveDependencies).distinctBy(_.title) :+ this
}

object Book {
  val chapterEntryParsers: Seq[ChapterEntryParser[_]] = Seq(Comment, StatementDefinition, TermDefinition, Axiom, Theorem)
  val bookEntryParsers: Seq[BookEntryParser] = Seq(Chapter, BookInclude, VariableDefinitions) ++ chapterEntryParsers

  private def lineParser(book: Book): Parser[Option[Book]] = {
    Parser.singleWordIfAny.mapFlatMap { entryType =>
      bookEntryParsers.find(_.name == entryType)
        .getOrElse(throw new Exception(s"Unrecognised entry '$entryType'"))
        .parser(book)
    }
  }

  private def parseToBook(book: Book, tokenizer: Tokenizer): Book = {
    val (updatedBookOption, nextTokenizer) = lineParser(book).parse(tokenizer)
    updatedBookOption match {
      case Some(updatedBook) =>
        parseToBook(updatedBook, nextTokenizer)
      case None =>
        book
    }
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
    parsedBooks: Seq[Book]
  ): (Seq[Book], Seq[PreParsedBook]) = {
    book.imports.foldLeft[Either[Seq[Book], String]](Left(Nil)) {
      case (Left(books), title) =>
        parsedBooks.find(_.title == title).map(b => Left(books :+ b)).getOrElse(Right(title))
      case (r @ Right(_), _) =>
        r
    } match {
      case Left(dependencies) =>
        val parsedBook = parseToBook(Book(book.title, book.path, dependencies), book.tokenizer)
        dependentBooks match {
          case dependentBook +: otherDependentBooks =>
            parseBook(dependentBook, otherDependentBooks, otherBooks, parsedBooks :+ parsedBook)
          case Nil =>
            (parsedBooks :+ parsedBook, otherBooks)
        }
      case Right(title) =>
        otherBooks.partition(_.title == title) match {
          case (Seq(dependency), otherOtherBooks) =>
            parseBook(dependency, book +: dependentBooks, otherOtherBooks, parsedBooks)
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

  private def parseBooks(books: Seq[PreParsedBook], parsedBooks: Seq[Book] = Nil): Seq[Book] = {
    books match {
      case Nil =>
        parsedBooks
      case book +: otherBooks =>
        val (moreParsedBooks, remainingUnparsedBooks) = parseBook(book, Nil, otherBooks, parsedBooks)
        parseBooks(remainingUnparsedBooks, moreParsedBooks)
    }
  }

  def fromDirectory(pathName: String): Seq[Book] = {
    val bookFilePaths = FileUtils.listFiles(new File(pathName), TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE)
      .asScala
      .map(_.toPath)
      .filter(_.getFileName.toString.endsWith(".book"))
      .toSeq
    val preparsedBooks = preparseBooks(bookFilePaths)
    parseBooks(preparsedBooks)
  }
}
