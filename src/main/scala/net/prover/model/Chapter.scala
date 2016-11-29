package net.prover.model

import scala.util.control.NonFatal

case class Chapter(title: String, summary: String, entries: Seq[ChapterEntry] = Nil) {
  val key: String = title.formatAsKey
}

trait ChapterEntry {
  def `type`: String
}

trait ChapterEntryParser[T <: ChapterEntry] {
  def name: String
  def parse(line: PartialLine, remainingLines: Seq[BookLine], context: Context): (T, Seq[BookLine])
  def addToBook(t: T, book: Book): Book

  def parseToBook(
    line: PartialLine,
    remainingLines: Seq[BookLine],
    book: Book
  ): (Book, Seq[BookLine]) = {
    parse(line, remainingLines, book.context)
      .mapLeft { model =>
        val updatedBook = addToBook(model, book)
        updatedBook.copy(chapters = updatedBook.chapters match {
          case previousChapters :+ lastChapter =>
            previousChapters :+ lastChapter.copy(entries = lastChapter.entries :+ model)
          case _ =>
            throw ParseException.withMessage("First entry of book must be a chapter", line.fullLine)
        })
      }
  }
}

trait SingleLineChapterEntryParser[T <: ChapterEntry] extends ChapterEntryParser[T] {
  def parse(line: PartialLine, context: Context): T

  override def parse(line: PartialLine, remainingLines: Seq[BookLine], context: Context): (T, Seq[BookLine]) = {
    try {
      (parse(line, context), remainingLines)
    } catch {
      case e: ParseException =>
        throw e
      case NonFatal(e) =>
        throw ParseException.fromCause(e, line.fullLine)
    }
  }
}
