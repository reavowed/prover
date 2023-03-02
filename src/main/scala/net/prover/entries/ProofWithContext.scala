package net.prover.entries

import net.prover.books.model.Book
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.{Chapter, ProvingContext}

case class ProofWithContext(book: Book, chapter: Chapter, theorem: Theorem, proof: Proof, proofIndex: Int, provingContext: ProvingContext)
