package net.prover.model

case class Chapter(title: String, summary: String, entries: Seq[ChapterEntry] = Nil) {
  val key: String = title.formatAsKey
}

object Chapter extends BookEntryParser {
  val name = "chapter"
  override def parser(book: Book, lines: Seq[BookLine]): Parser[(Book, Seq[BookLine])] = {
    for {
      title <- Parser.allRemaining
    } yield {
      lines match {
        case BookLine(summary, _, _, _) +: linesAfterSummary =>
          val updatedBook = book.copy(chapters = book.chapters :+ Chapter(title, summary))
          (updatedBook, linesAfterSummary)
        case _ =>
          throw new Exception("Chapter summary missing")
      }
    }
  }
}

abstract class ChapterEntry(chapterEntryParser: ChapterEntryParser[_]) {
  val `type`: String = chapterEntryParser.name
}
