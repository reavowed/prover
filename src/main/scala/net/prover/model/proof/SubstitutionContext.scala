package net.prover.model.proof

trait SubstitutionContext {
  def externalDepth: Int
}

object SubstitutionContext {
  def withDepth(depth: Int) = new SubstitutionContext {
    override def externalDepth: Int = depth
  }
  val outsideProof: SubstitutionContext = withDepth(0)
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): SubstitutionContext = stepProvingContext.stepContext
}
