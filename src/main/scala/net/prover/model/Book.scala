package net.prover.model

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

case class Book(
  title: String,
  connectives: Seq[Connective] = Nil,
  rules: Seq[Rule] = Nil,
  theorems: Seq[Theorem] = Nil,
  definitions: Seq[Definition] = Nil)

trait BookEntryParser[T] {
  def name: String
  def parse(firstLine: String, remainingLines: Seq[String], book: Book): (T, Seq[String])
  def addToBook(t: T, book: Book): Book

  def parseToBook(firstLine: String, remainingLines: Seq[String], book: Book): (Book, Seq[String]) = {
    parse(firstLine, remainingLines, book).mapLeft(addToBook(_, book))
  }
}

trait SingleLineBookEntryParser[T] extends BookEntryParser[T] {
  def parse(line: String, book: Book): T

  override def parse(firstLine: String, remainingLines: Seq[String], book: Book): (T, Seq[String]) = {
    (parse(firstLine, book), remainingLines)
  }
}

object Book {

  val bookEntryParsers: Seq[BookEntryParser[_]] = Seq(Connective, Definition, Rule, Theorem)

  private def addLinesToBook(lines: Seq[String], book: Book): Book = {
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
    val lines = s.lines.map(_.trim).filter(!_.isEmpty).filter(!_.startsWith("#")).toList
    lines match {
      case WordAndRemainingText("book", title) +: otherLines =>
        addLinesToBook(otherLines, Book(title))
      case _ =>
        throw new Exception("Book must start with title line")
    }
  }

  def fromFile(pathText: String): Book = {
    val path = Paths.get("book.txt")
    val bookText = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
    parse(bookText)
  }
}
