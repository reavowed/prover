package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model._

case class TermVariable(name: String, depth: Int) extends ExpressionVariable[Term] with Term {
  override def increaseDepth(additionalDepth: Int) = {
    if (depth + additionalDepth < 0) throw new Exception("Invalid depth increase")
    copy(depth = depth + additionalDepth)
  }
  override def substitutionsLens = GenLens[Substitutions](_.terms)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.terms)

  override def calculateApplicatives(baseArguments: Seq[Term], substitutions: Substitutions) = {
    super[Term].calculateApplicatives(baseArguments, substitutions) ++
      super[ExpressionVariable].calculateApplicatives(baseArguments, substitutions)
  }
  override def makeApplicative(names: Seq[String]) = {
    Some(FunctionApplication(name, names.mapWithIndex{ (n, i) => FunctionParameter(n, i) }, depth + 1))
  }

  override def toString: String = name
  override def serialized: String = name
}
