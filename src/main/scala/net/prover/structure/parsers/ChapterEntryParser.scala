package net.prover.structure.parsers

import net.prover.model.Parser
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.ChapterEntry

trait ChapterEntryParser {
  def name: String
  def parser(implicit context: EntryContext): Parser[ChapterEntry]
}
