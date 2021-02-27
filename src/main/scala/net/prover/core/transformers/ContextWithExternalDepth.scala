package net.prover.core.transformers

import net.prover.model.proof.SubstitutionContext

case class ContextWithExternalDepth(externalDepth: Int)

object ContextWithExternalDepth {
  val zero = ContextWithExternalDepth(0)

  implicit def fromStepContext(substitutionContext: SubstitutionContext): ContextWithExternalDepth = ContextWithExternalDepth(substitutionContext.externalDepth)
  implicit def fromImplicitStepContext(implicit substitutionContext: SubstitutionContext): ContextWithExternalDepth = ContextWithExternalDepth(substitutionContext.externalDepth)
}
