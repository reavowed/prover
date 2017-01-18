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
    transitiveDependencies.map(_.context).reduce(_ + _) + context
  }
}

case class BookLine(text: String, number: Int, bookTitle: String) {
  def splitFirstWord: (String, PartialLine) = {
    text.splitFirstWord.mapRight(PartialLine(_, this))
  }
}
case class PartialLine(remainingText: String, fullLine: BookLine) {
  def splitFirstWord: (String, PartialLine) = {
    remainingText.splitFirstWord.mapRight(PartialLine(_, fullLine))
  }
  def tail: PartialLine = {
    copy(remainingText = remainingText.tail.trim())
  }
  def toEndOfParens: (String, PartialLine) = {
    val (_, pre, post) = remainingText.foldLeft((1, "", "")) { case ((count, pre, post), c) =>
      if (count == 0 || (count == 1 &&  c == ')'))
        (0, pre, post + c)
      else {
        val newCount = if (c == '(')
          count + 1
        else if (c == ')')
          count - 1
        else
          count
        (newCount, pre + c, post)
      }
    }
    (pre, PartialLine(post, fullLine))
  }
  def isEmpty: Boolean = remainingText.isEmpty
  def nonEmpty: Boolean = remainingText.nonEmpty
}

object Book {

  val entryParsers: Seq[ChapterEntryParser[_]] = Seq(Comment, Connective, Quantifier, Predicate, Rule, Theorem, Axiom, TermDefinition)

  private def addLinesToBook(lines: Seq[BookLine], book: Book): Book = {
    lines match {
      case WordAndRemainingText("chapter", PartialLine(chapterTitle, _)) +: linesAfterChapterDefinition =>
        linesAfterChapterDefinition match {
          case BookLine(chapterSummary, _, _) +: linesAfterChapterSummary =>
            addLinesToBook(
              linesAfterChapterSummary,
              book.copy(chapters = book.chapters :+ Chapter(chapterTitle, chapterSummary)))
        }
      case WordAndRemainingText("include", PartialLine(includePathText, _)) +: linesAfterInclude =>
        val includeLines = getIncludeLines(includePathText, book)
        addLinesToBook(includeLines ++ linesAfterInclude, book)
      case WordAndRemainingText(entryType, restOfLine) +: moreLines =>
        val parser = entryParsers.find(_.name == entryType)
          .getOrElse(throw ParseException.withMessage(s"Unrecognised type '$entryType'", lines.head))
        val (updatedBook, remainingLines) = parser.parseToBook(restOfLine, moreLines, book)
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

  private def getIncludeLines(pathText: String, book: Book): Seq[BookLine] = {
    val path = book.path.getParent.resolve(pathText)
    val plainLines = getPlainLinesWithIndices(path)
    createBookLines(plainLines, book.title)
  }

  private def getPlainLinesWithIndices(path: Path): Seq[(String, Int)] = {
    val bookText = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
    bookText.lines.zipWithIndex.filter(!_._1.isEmpty).filter(!_._1.startsWith("#")).toList
  }

  private def createBookLines(plainLines: Seq[(String, Int)], bookTitle: String): Seq[BookLine] = {
    plainLines map { case(lineText, lineIndex) =>
      BookLine(lineText, lineIndex + 1, bookTitle)
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
      val bookLinesAfterTitle = createBookLines(plainLinesAfterTitle, title)
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
            parseBook(dependentBook, otherDependentBooks, otherBooks, parsedBook +: parsedBooks)
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
