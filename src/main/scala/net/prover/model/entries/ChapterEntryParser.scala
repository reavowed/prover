package net.prover.model.entries

import net.prover.entries.EntryParsingContext
import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(implicit entryParsingContext: EntryParsingContext): Parser[ChapterEntry]
}
