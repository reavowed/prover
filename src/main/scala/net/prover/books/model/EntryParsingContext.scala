package net.prover.books.model

import net.prover.books.reading.ProofFileReader
import net.prover.model.EntryContext
import net.prover.model.entries.ChapterEntry

case class EntryParsingContext(entryContext: EntryContext, proofFileReader: ProofFileReader)
