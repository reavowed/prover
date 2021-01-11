package net.prover.core.transformers

case class ContextWithExternalDepth(externalDepth: Int)

object ContextWithExternalDepth {
  val zero = ContextWithExternalDepth(0)
}
