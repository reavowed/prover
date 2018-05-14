package net.prover.model.entries

import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(chapterTitle: String, bookTitle: String, getKey: String => String)(implicit context: ParsingContext): Parser[ChapterEntry]
}

object ChapterEntryParser {
  trait WithoutKey extends ChapterEntryParser {
    def parser(chapterTitle: String, bookTitle: String)(implicit context: ParsingContext): Parser[ChapterEntry]
    def parser(chapterTitle: String, bookTitle: String, getKey: String => String)(implicit context: ParsingContext): Parser[ChapterEntry] = {
      parser(chapterTitle, bookTitle)
    }
  }
}
