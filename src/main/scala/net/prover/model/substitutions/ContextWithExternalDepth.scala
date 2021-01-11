package net.prover.model.substitutions

trait ContextWithExternalDepth {
  def externalDepth: Int
}
object ContextWithExternalDepth {
  val outsideProof: ContextWithExternalDepth = new ContextWithExternalDepth {
    override def externalDepth: Int = 0
  }
}
