package net.prover.entries

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.{Book, Chapter, ProvingContext}

case class ProofWithContext(book: Book, chapter: Chapter, theorem: Theorem, proof: Proof, proofIndex: Int, provingContext: ProvingContext)
