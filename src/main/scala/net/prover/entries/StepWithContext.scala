package net.prover.entries

import net.prover.model.ProvingContext
import net.prover.model.proof.{Step, StepContext, StepProvingContext}

import scala.reflect.ClassTag

case class TypedStepWithContext[+T <: Step : ClassTag](
  step: T,
  index: Int,
  stepContext: StepContext,
  parentContext: StepsWithContext
) {
  implicit def provingContext: ProvingContext = parentContext.provingContext
  implicit def stepProvingContext: StepProvingContext = StepProvingContext(stepContext, provingContext)
  def globalContext: GlobalContext = parentContext.globalContext

  def nextSibling: Option[StepWithContext] = parentContext.atIndex(index + 1)
  def forSubsteps(step: Step.WithSubsteps): StepsWithContext = {
    StepsWithContext(step.substeps, stepContext.stepReference, step.specifyStepContext(stepContext), parentContext.proofWithContext)
  }
}
