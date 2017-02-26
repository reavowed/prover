package net.prover.model

object BookInclude extends BookEntryParser {
  override def name: String = "include"
  override def parser(book: Book, lines: Seq[BookLine]): Parser[(Book, Seq[BookLine])] = {
    for {
      pathText <- Parser.allRemaining
    } yield {
      (book, getIncludeLines(pathText, book) ++ lines)
    }
  }

  private def getIncludeLines(pathText: String, book: Book): Seq[BookLine] = {
    val path = book.path.getParent.resolve(pathText)
    val plainLines = Book.getPlainLinesWithIndices(path)
    Book.createBookLines(plainLines, book.title, path.getFileName.toString)
  }
}
