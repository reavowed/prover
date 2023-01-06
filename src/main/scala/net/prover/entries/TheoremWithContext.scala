package net.prover.entries

import net.prover.model.entries.Theorem
import net.prover.model.{Book, Chapter, EntryContext, ProvingContext}

case class TheoremWithContext(book: Book, chapter: Chapter, theorem: Theorem, provingContext: ProvingContext)
