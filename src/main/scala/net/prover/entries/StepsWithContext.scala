package net.prover.entries

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.model.{Book, Chapter}

case class StepsWithContext(book: Book, chapter: Chapter, theorem: Theorem, proof: Proof, proofIndex: Int, steps: Seq[Step], outerStepProvingContext: StepProvingContext)
