package net.prover.entries

import net.prover.model.ProvingContext
import net.prover.model.proof.{Step, StepContext, StepProvingContext, StepReference}

import scala.reflect.ClassTag

case class StepsWithContext(
  steps: Seq[Step],
  outerReference: StepReference,
  outerStepContext: StepContext,
  proofWithContext: ProofWithContext
) {
  def globalContext: GlobalContext = proofWithContext.globalContext
  implicit def provingContext: ProvingContext = proofWithContext.provingContext
  def outerStepProvingContext: StepProvingContext = StepProvingContext(outerStepContext, provingContext)

  def atIndex(index: Int): Option[StepWithContext] = {
    steps.splitAtIndexIfValid(index).map { case (before, step, _) => atChild(before, step) }
  }
  def atChild[T <: Step : ClassTag](before: Seq[Step], step: T): TypedStepWithContext[T] = {
    val index = before.length
    TypedStepWithContext[T](step, index, outerStepContext.addSteps(before).atIndex(index), this)
  }
}
