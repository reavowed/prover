package net.prover.model.entries

import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[ChapterEntry]
}

object ChapterEntryParser {
  trait WithoutKey extends ChapterEntryParser {
    def parser(implicit context: ParsingContext): Parser[ChapterEntry]
    def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[ChapterEntry] = {
      parser
    }
  }
}
