package net.prover.model

trait ChapterEntryParser[T <: ChapterEntry] extends BookEntryParser {
  def parser(implicit context: Context): Parser[T]
  def addToContext(t: T, context: Context): Context

  override def parser(book: Book): Parser[Book] = {
    parser(book.fullContext) map { entry =>
      val updatedContext = addToContext(entry, book.context)
      val updatedBook = book.copy(
        context = updatedContext,
        chapters = book.chapters match {
          case previousChapters :+ lastChapter =>
            previousChapters :+ lastChapter.copy(entries = lastChapter.entries :+ entry)
          case _ =>
            throw new Exception("First entry of book must be a chapter")
        })
      updatedBook
    }
  }
}
