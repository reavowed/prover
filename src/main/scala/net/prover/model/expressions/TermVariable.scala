package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model._

case class TermVariable(name: String) extends ExpressionVariable[Term] with Term {
  override def substitutionsLens = GenLens[Substitutions](_.terms)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.terms)

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(Term, Substitutions)] = {
    super[Term].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
      super[ExpressionVariable].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
  }
}
