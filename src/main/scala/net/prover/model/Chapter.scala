package net.prover.model

import scala.util.control.NonFatal

case class Chapter(title: String, summary: String, entries: Seq[ChapterEntry] = Nil) {
  val key: String = title.formatAsKey
}

abstract class ChapterEntry(chapterEntryParser: ChapterEntryParser[_]) {
  val `type`: String = chapterEntryParser.name
}

trait ChapterEntryParser[T <: ChapterEntry] {
  def name: String
  def parse(line: PartialLine, remainingLines: Seq[BookLine], context: Context): (T, Seq[BookLine])
  def addToContext(t: T, context: Context): Context

  def parseToBook(
    line: PartialLine,
    remainingLines: Seq[BookLine],
    book: Book
  ): (Book, Seq[BookLine]) = {
    parse(line, remainingLines, book.fullContext)
      .mapLeft { model =>
        val updatedContext = addToContext(model, book.context)
        book.copy(
          context = updatedContext,
          chapters = book.chapters match {
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
