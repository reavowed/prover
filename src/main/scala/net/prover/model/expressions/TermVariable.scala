package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model._

case class TermVariable(name: String, depth: Int) extends ExpressionVariable[Term] with Term {
  override def setDepth(newDepth: Int) = {
    copy(depth = newDepth)
  }
  override def substitutionsLens = GenLens[Substitutions](_.terms)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.terms)

  override def calculateApplicatives(baseArguments: ArgumentList, substitutions: Substitutions) = {
    super[Term].calculateApplicatives(baseArguments, substitutions) ++
      super[ExpressionVariable].calculateApplicatives(baseArguments, substitutions)
  }
  override def toString: String = name
  override def serialized: String = name
}
