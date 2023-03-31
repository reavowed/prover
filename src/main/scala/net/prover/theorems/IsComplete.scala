package net.prover.theorems

import net.prover.entries.{StepWithContext, TheoremWithContext}
import net.prover.model.proof.Step
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object IsComplete extends RecursiveStepFinder[Boolean]()(booleanInstance.conjunction) {
  def apply(theoremWithContext: TheoremWithContext): Boolean = {
    theoremWithContext.proofsWithContext.exists(p => apply(p.stepsWithContext))
  }

  override def getFromTarget(step: Step.Target, stepWithContext: StepWithContext): Boolean = {
    false
  }

  override def getFromAssertion(step: Step.Assertion, stepWithContext: StepWithContext): Boolean = {
    step.premises.forall(_.isComplete) &&
      stepWithContext.globalContext.definitions.isInferenceComplete(step.inference)
  }

  override def getFromNaming(step: Step.Naming, stepWithContext: StepWithContext): Boolean = {
    super.getFromNaming(step, stepWithContext) &&
      step.premises.forall(_.isComplete) &&
      stepWithContext.globalContext.definitions.isInferenceComplete(step.inference)
  }
}
