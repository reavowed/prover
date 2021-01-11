package net.prover.model.proof

import net.prover.model.substitutions.ContextWithExternalDepth

trait SubstitutionContext extends ContextWithExternalDepth {
  def externalDepth: Int
}

object SubstitutionContext {
  def withDepth(depth: Int) = new SubstitutionContext {
    override def externalDepth: Int = depth
  }
  def withExtraParameter(implicit substitutionContext: SubstitutionContext): SubstitutionContext = withExtraParameters(1)
  def withExtraParameters(number: Int)(implicit substitutionContext: SubstitutionContext): SubstitutionContext = withDepth(substitutionContext.externalDepth + number)
  val outsideProof: SubstitutionContext = withDepth(0)
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): SubstitutionContext = stepProvingContext.stepContext
}
