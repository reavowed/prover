package net.prover.model.proof

trait SubstitutionContext {
  def externalDepth: Int
}

object SubstitutionContext {
  private def withDepth(depth: Int) = new SubstitutionContext {
    override def externalDepth: Int = depth
  }
  def withExtraParameter(implicit substitutionContext: SubstitutionContext): SubstitutionContext = withDepth(substitutionContext.externalDepth + 1)
  val outsideProof: SubstitutionContext = withDepth(0)
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): SubstitutionContext = stepProvingContext.stepContext
}
