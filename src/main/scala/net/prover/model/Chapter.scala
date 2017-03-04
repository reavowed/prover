package net.prover.model

case class Chapter(title: String, summary: String, entries: Seq[ChapterEntry] = Nil) {
  val key: String = title.formatAsKey
}

object Chapter extends BookEntryParser {
  val name = "chapter"
  override def parser(book: Book): Parser[Book] = {
    for {
      title <- Parser.toEndOfLine
      summary <- Parser.toEndOfLine
    } yield {
      book.copy(chapters = book.chapters :+ Chapter(title, summary))
    }
  }
}

abstract class ChapterEntry(chapterEntryParser: ChapterEntryParser[_]) {
  val `type`: String = chapterEntryParser.name
}
