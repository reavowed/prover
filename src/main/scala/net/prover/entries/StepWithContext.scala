package net.prover.entries

import net.prover.books.model.Book
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.model.Chapter

case class StepWithContext(book: Book, chapter: Chapter, theorem: Theorem, proof: Proof, proofIndex: Int, step: Step, stepProvingContext: StepProvingContext) {
  def forSubsteps(step: Step.WithSubsteps): StepsWithContext = {
    StepsWithContext(book, chapter, theorem, proof, proofIndex, step.substeps, stepProvingContext.updateStepContext(step.specifyStepContext))
  }
}
