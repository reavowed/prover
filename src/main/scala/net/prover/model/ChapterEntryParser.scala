package net.prover.model

trait ChapterEntryParser[T <: ChapterEntry] extends BookEntryParser {
  def parser(lines: Seq[BookLine])(implicit context: Context): Parser[(T, Seq[BookLine])]
  def addToContext(t: T, context: Context): Context

  override def parser(book: Book, lines: Seq[BookLine]): Parser[(Book, Seq[BookLine])] = {
    parser(lines)(book.fullContext) map { case (entry, updatedLines) =>
      val updatedContext = addToContext(entry, book.context)
      val updatedBook = book.copy(
        context = updatedContext,
        chapters = book.chapters match {
          case previousChapters :+ lastChapter =>
            previousChapters :+ lastChapter.copy(entries = lastChapter.entries :+ entry)
          case _ =>
            throw new Exception("First entry of book must be a chapter")
        })
      (updatedBook, updatedLines)
    }
  }
}

trait SingleLineChapterEntryParser[T <: ChapterEntry] extends ChapterEntryParser[T] {
  def parser(implicit context: Context): Parser[T]
  override def parser(lines: Seq[BookLine])(implicit context: Context): Parser[(T, Seq[BookLine])] = {
    parser.map(entry => (entry, lines))
  }
}
