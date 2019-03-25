package net.prover.model.entries

import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[ChapterEntry]
}
