package net.prover.entries

import net.prover.model.ProvingContext
import net.prover.model.proof.{Step, StepContext, StepReference}

case class StepsWithContext(
  steps: Seq[Step],
  outerReference: StepReference,
  outerStepContext: StepContext,
  proofWithContext: ProofWithContext
) {
  implicit def provingContext: ProvingContext = proofWithContext.provingContext
  def atIndex(index: Int): Option[StepWithContext] = {
    steps.splitAtIndexIfValid(index).map { case (before, step, _) => atChild(before, step) }
  }
  def atChild(before: Seq[Step], step: Step): StepWithContext = {
    val index = before.length
    TypedStepWithContext[Step](step, index, outerStepContext.addSteps(before).atIndex(index), this)
  }
}
