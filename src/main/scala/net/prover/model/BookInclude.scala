package net.prover.model

object BookInclude extends BookEntryParser {
  override def name: String = "include"
  override def parse(remainingLine: PartialLine, otherLines: Seq[BookLine], book: Book): (Book, Seq[BookLine]) = {
    val pathText = remainingLine.remainingText
    (book, getIncludeLines(pathText, book) ++ otherLines)
  }

  private def getIncludeLines(pathText: String, book: Book): Seq[BookLine] = {
    val path = book.path.getParent.resolve(pathText)
    val plainLines = Book.getPlainLinesWithIndices(path)
    Book.createBookLines(plainLines, book.title, path.getFileName.toString)
  }
}
