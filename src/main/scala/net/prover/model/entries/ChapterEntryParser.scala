package net.prover.model.entries

import net.prover.books.model.EntryParsingContext
import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(implicit context: EntryParsingContext): Parser[ChapterEntry]
}
