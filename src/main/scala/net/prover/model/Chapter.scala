package net.prover.model

case class Chapter(title: String, summary: String, entries: Seq[ChapterEntry] = Nil) {
  val key: String = title.formatAsKey
}

object Chapter extends BookEntryParser {
  val name = "chapter"
  override def parse(remainingLine: PartialLine, otherLines: Seq[BookLine], book: Book): (Book, Seq[BookLine]) = {
    val title = remainingLine.remainingText
    otherLines match {
      case BookLine(summary, _, _, _) +: linesAfterSummary =>
        val updatedBook = book.copy(chapters = book.chapters :+ Chapter(title, summary))
        (updatedBook, linesAfterSummary)
      case _ =>
        remainingLine.throwParseException("Chapter summary missing")
    }
  }
}

abstract class ChapterEntry(chapterEntryParser: ChapterEntryParser[_]) {
  val `type`: String = chapterEntryParser.name
}
