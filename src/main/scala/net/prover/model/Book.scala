package net.prover.model

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

case class Book(
  title: String,
  chapters: Seq[Chapter] = Nil,
  connectives: Seq[Connective] = Nil,
  rules: Seq[Rule] = Nil,
  theorems: Seq[Theorem] = Nil,
  definitions: Seq[Definition] = Nil)

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

  val entryParsers: Seq[ChapterEntryParser[_]] = Seq(Comment, Connective, Definition, Rule, Theorem)

  private def addLinesToBook(lines: Seq[BookLine], book: Book): Book = {
    lines match {
      case WordAndRemainingText("chapter", PartialLine(chapterTitle, _)) +: linesAfterChapterDefinition =>
        linesAfterChapterDefinition match {
          case BookLine(chapterSummary, _) +: linesAfterChapterSummary =>
            addLinesToBook(
              linesAfterChapterSummary,
              book.copy(chapters = book.chapters :+ Chapter(chapterTitle, chapterSummary)))
        }
      case (line @ WordAndRemainingText(entryType, restOfLine)) +: moreLines =>
        val parser = entryParsers.find(_.name == entryType)
          .getOrElse(throw new Exception(s"Unrecognised type '$entryType'"))
        val (updatedBook, remainingLines) = parser.parseToBook(restOfLine, moreLines, book)
        addLinesToBook(remainingLines, updatedBook)
      case Nil =>
        book
    }
  }

  def parse(s: String): Book = {
    val lines = s.lines.zipWithIndex.map {
      case (line, index) => BookLine(line, index + 1)
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
