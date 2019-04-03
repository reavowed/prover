package net.prover.model.entries

import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(implicit context: EntryContext): Parser[ChapterEntry]
}
