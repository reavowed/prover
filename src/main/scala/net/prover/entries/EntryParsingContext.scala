package net.prover.entries

import net.prover.books.reading.ProofFileReader
import net.prover.model.definitions.Definitions
import net.prover.model.{AvailableEntries, ProvingContext}

case class EntryParsingContext(
  bookTitle: String,
  chapterTitle: String,
  proofFileReader: ProofFileReader)(
  implicit val availableEntries: AvailableEntries)
{
  implicit lazy val provingContext: ProvingContext = ProvingContext(availableEntries, Definitions(availableEntries))
}
