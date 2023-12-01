package net.prover.proving

import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, ProofUpdateProps, StepDefinition, StepInsertionProps}
import net.prover.proving.stepReplacement.InsertStepBeforeChain

import scala.util.Try

object ProveNewTarget {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepReference: PathData,
    definition: StepDefinition)(
    implicit bookService: BookService
  ): Try[ProofUpdateProps[StepInsertionProps]] = {
    InsertStepBeforeChain(bookKey, chapterKey, theoremKey, proofIndex, stepReference) { stepProvingContext =>
      implicit val spc = stepProvingContext
      for {
        (newStep, targets) <- CreateProofStep(
          definition,
          definition.parseIntendedConclusion,
          Nil)
      } yield targets :+ newStep
    }
  }
}
