package net.prover.model.proof

trait SubstitutionContext {
  def externalDepth: Int
}

object SubstitutionContext {
  val outsideProof: SubstitutionContext = new SubstitutionContext {
    override def externalDepth: Int = 0
  }
}
