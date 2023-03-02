package net.prover.entries

import net.prover.books.model.Book
import net.prover.model.entries.Theorem
import net.prover.model.{Chapter, EntryContext, ProvingContext}

case class TheoremWithContext(book: Book, chapter: Chapter, theorem: Theorem, provingContext: ProvingContext)
