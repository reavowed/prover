package net.prover.entries

import net.prover.model.proof.{Step, StepContext}
import net.prover.model.{AvailableEntries, ProvingContext}

import scala.reflect.ClassTag

case class TypedStepWithContext[+T <: Step : ClassTag](
    step: T,
    proofWithContext: ProofWithContext)(
    implicit val stepContext: StepContext)
{
  def globalContext: GlobalContext = proofWithContext.globalContext
  implicit def provingContext: ProvingContext = stepContext.provingContext
  def availableEntries: AvailableEntries = provingContext.availableEntries

  def withStep[NewStep <: Step : ClassTag](newStep: NewStep): TypedStepWithContext[NewStep] = {
    copy(step = newStep)
  }
  def forSubsteps(step: Step.WithSubsteps): StepsWithContext = {
    StepsWithContext(
      step.substeps,
      step.specifyStepContext(stepContext),
      proofWithContext)
  }
}
