package net.prover.model.proof

import net.prover.entries.{StepWithContext, StepsWithContext}

trait SubstitutionContext {
  def externalDepth: Int
}

object SubstitutionContext {
  def withDepth(depth: Int) = new SubstitutionContext {
    override def externalDepth: Int = depth
  }
  def withExtraParameter(implicit substitutionContext: SubstitutionContext): SubstitutionContext = withExtraParameters(1)
  def withExtraParameters(number: Int)(implicit substitutionContext: SubstitutionContext): SubstitutionContext = withDepth(substitutionContext.externalDepth + number)
  val outsideProof: SubstitutionContext = withDepth(0)

  implicit def fromStepWithContext(stepWithContext: StepWithContext): SubstitutionContext = stepWithContext.stepContext
  implicit def implicitlyFromStepWithContext(implicit stepWithContext: StepWithContext): SubstitutionContext = fromStepWithContext(stepWithContext)
  implicit def fromStepsWithContext(implicit stepsWithContext: StepsWithContext): SubstitutionContext = stepsWithContext.outerStepContext
}
