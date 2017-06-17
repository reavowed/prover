package net.prover.model

trait ChapterEntryParser[T <: ChapterEntry] extends BookEntryParser {
  def parser(book: Book, chapter: Chapter)(implicit context: Context): Parser[T]
  def addToContext(t: T, context: Context): Context

  override def parser(book: Book): Parser[Book] = {
    book.chapters match {
      case previousChapters :+ currentChapter =>
        parser(book, currentChapter)(book.fullContext) map { entry =>
          book.copy(
            context = addToContext(entry, book.context),
            chapters = previousChapters :+ currentChapter.copy(entries = currentChapter.entries :+ entry))
        }
      case _ =>
        throw new Exception("First entry of book must be a chapter")

    }
  }
}
