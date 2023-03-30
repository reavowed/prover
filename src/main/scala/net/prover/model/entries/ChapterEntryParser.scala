package net.prover.model.entries

import net.prover.books.reading.ProofFileReader
import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(implicit entryContext: EntryContext, proofFileReader: ProofFileReader): Parser[ChapterEntry]
}
