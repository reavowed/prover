package net.prover.model

import net.prover.model.entries.{BookEntryParser, ChapterEntryParser, Theorem}

case class Chapter(
  title: String,
  summary: String,
  bookKey: String,
  bookTitle: String,
  entries: Seq[ChapterEntry] = Nil)
{
  val key: String = title.formatAsKey

  def theoremCache: Seq[Theorem] = {
    entries.ofType[Theorem]
  }
}

object Chapter extends BookEntryParser {
  val name = "chapter"
  override def parser(book: Book): Parser[Book] = {
    for {
      title <- Parser.toEndOfLine
      summary <- Parser.toEndOfLine
    } yield {
      book.copy(chapters = book.chapters :+ Chapter(title, summary, book.key, book.title))
    }
  }
}

abstract class ChapterEntry(chapterEntryParser: ChapterEntryParser[_]) {
  val `type`: String = chapterEntryParser.name
}
