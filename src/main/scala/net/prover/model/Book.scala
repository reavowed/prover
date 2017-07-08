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
    chapters: Seq[Chapter],
    context: Context) {
  val key: String = title.formatAsKey

  lazy val fullContext: Context = {
    context.combine(transitiveDependencies.map(_.context))
  }

  def theoremCache: Seq[Theorem] = {
    chapters.flatMap(_.theoremCache)
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
      .optionalWord("import")
      .mapFlatMap(_ => Parser.toEndOfLine)
      .whileDefined
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
    linesParser(Book(book.title, book.path, dependencies, Nil, Context.empty.withTheoremCache(theoremCache)))
      .parse(book.tokenizer)._1
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
