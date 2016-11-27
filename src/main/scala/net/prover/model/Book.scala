package net.prover.model

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.util.control.NonFatal

case class Book(
  title: String,
  connectives: Seq[Connective] = Nil,
  rules: Seq[Rule] = Nil,
  theorems: Seq[Theorem] = Nil,
  definitions: Seq[Definition] = Nil)

trait BookEntryParser[T] {
  def name: String
  def parse(line: PartialLine, remainingLines: Seq[BookLine], book: Book): (T, Seq[BookLine])
  def addToBook(t: T, book: Book): Book

  def parseToBook(line: PartialLine, remainingLines: Seq[BookLine], book: Book): (Book, Seq[BookLine]) = {
    parse(line, remainingLines, book).mapLeft(addToBook(_, book))
  }
}

trait SingleLineBookEntryParser[T] extends BookEntryParser[T] {
  def parse(line: PartialLine, book: Book): T

  override def parse(line: PartialLine, remainingLines: Seq[BookLine], book: Book): (T, Seq[BookLine]) = {
    try {
      (parse(line, book), remainingLines)
    } catch {
      case e: ParseException =>
        throw e
      case NonFatal(e) =>
        throw ParseException.fromCause(e, line.fullLine)
    }
  }
}

case class BookLine(text: String, number: Int) {
  def splitFirstWord: (String, PartialLine) = {
    text.splitFirstWord.mapRight(PartialLine(_, this))
  }
}
case class PartialLine(remainingText: String, fullLine: BookLine) {
  def splitFirstWord: (String, PartialLine) = {
    remainingText.splitFirstWord.mapRight(PartialLine(_, fullLine))
  }
  def splitWords: Seq[String] = {
    remainingText.splitByWhitespace()
  }
}

object Book {

  val bookEntryParsers: Seq[BookEntryParser[_]] = Seq(Connective, Definition, Rule, Theorem)

  private def addLinesToBook(lines: Seq[BookLine], book: Book): Book = {
    lines match {
      case nextLine +: moreLines =>
        val (entryType, restOfLine) = nextLine.splitFirstWord
        val parser = bookEntryParsers.find(_.name == entryType)
          .getOrElse(throw new Exception(s"Unrecognised type '$entryType'"))
        val (updatedBook, remainingLines) = parser.parseToBook(restOfLine, moreLines, book)
        addLinesToBook(remainingLines, updatedBook)
      case Nil =>
        book
    }
  }

  def parse(s: String): Book = {
    val lines = s.lines.zipWithIndex.map {
      case (line, number) => BookLine(line, number)
    }.filter(!_.text.isEmpty).filter(!_.text.startsWith("#")).toList
    lines match {
      case firstLine +: otherLines =>
        val book = firstLine match {
          case WordAndRemainingText("book", remainingLine) =>
            val title = remainingLine.remainingText
            Book(title)
          case _ =>
            throw ParseException.withMessage("First line of book must be a book definition", firstLine)
        }
        addLinesToBook(otherLines, book)
      case _ =>
        throw new Exception("Book must at least have a title line")
    }
  }

  def fromFile(pathText: String): Book = {
    val path = Paths.get(pathText)
    val bookText = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
    parse(bookText)
  }
}
