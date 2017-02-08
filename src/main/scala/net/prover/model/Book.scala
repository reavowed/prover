package net.prover.model

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

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

  protected def transitiveDependencies: Seq[Book] = dependencies.flatMap(_.transitiveDependencies).distinctBy(_.title) :+ this

  lazy val fullContext: Context = {
    context.combine(transitiveDependencies.map(_.context))
  }
}

object Book {
  val chapterEntryParsers: Seq[ChapterEntryParser[_]] = Seq(Comment, StatementDefinition, TermDefinition, Axiom, Theorem)
  val bookEntryParsers: Seq[BookEntryParser] = Seq(Chapter, BookInclude, VariableDefinitions) ++ chapterEntryParsers

  private def addLinesToBook(lines: Seq[BookLine], book: Book): Book = {
    lines match {
      case WordAndRemainingText(entryType, restOfLine) +: moreLines =>
        val parser = bookEntryParsers.find(_.name == entryType)
          .getOrElse(throw ParseException.withMessage(s"Unrecognised entry '$entryType'", lines.head))
        val (updatedBook, remainingLines) = parser.parse(restOfLine, moreLines, book)
        addLinesToBook(remainingLines, updatedBook)
      case Nil =>
        book
    }
  }

  private def readImports(nextLines: Seq[BookLine], imports: Seq[String] = Nil): (Seq[String], Seq[BookLine]) = {
    nextLines match {
      case WordAndRemainingText("import", PartialLine(importTitle, _)) +: remainingLines =>
        readImports(remainingLines, imports :+ importTitle)
      case _ =>
        (imports, nextLines)
    }
  }

  private case class PreParsedBook(title: String, path: Path, imports: Seq[String], lines: Seq[BookLine])

  def getPlainLinesWithIndices(path: Path): Seq[(String, Int)] = {
    val bookText = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
    bookText.lines.zipWithIndex.filter(!_._1.isEmpty).filter(!_._1.startsWith("#")).toList
  }

  def createBookLines(plainLines: Seq[(String, Int)], bookTitle: String, fileName: String): Seq[BookLine] = {
    plainLines map { case(lineText, lineIndex) =>
      BookLine(lineText, lineIndex + 1, bookTitle, fileName)
    }
  }

  private def preparseBooks(bookFiles: Seq[File]): Seq[PreParsedBook] = {
    bookFiles.map { file =>
      val path = file.toPath
      val plainLinesWithIndices = getPlainLinesWithIndices(path)
      val (title, plainLinesAfterTitle) = plainLinesWithIndices match {
        case (WordAndRemainingText("book", bookTitle), _) +: remainingLines =>
          (bookTitle, remainingLines)
        case _ =>
          throw new Exception("Book must start with a title line")
      }
      val bookLinesAfterTitle = createBookLines(plainLinesAfterTitle, title, path.getFileName.toString)
      val (imports, linesAfterImports) = readImports(bookLinesAfterTitle)
      PreParsedBook(title, path, imports, linesAfterImports)
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
        val parsedBook = addLinesToBook(
          book.lines,
          Book(book.title, book.path, dependencies))
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
    val bookFiles = FileUtils.listFiles(new File(pathName), TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE)
      .asScala
      .filter(_.getPath.endsWith(".book"))
      .toSeq
    val preparsedBooks = preparseBooks(bookFiles)
    parseBooks(preparsedBooks)
  }
}
