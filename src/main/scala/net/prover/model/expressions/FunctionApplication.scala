package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model.Substitutions

case class FunctionApplication(variableName: String, arguments: Seq[Term], depth: Int) extends ExpressionApplication[Term] with Term {
  override def substitutionsLens = GenLens[Substitutions](_.functions)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.functions)

  def update(newArguments: Seq[Term], newDepth: Int) = FunctionApplication(variableName, newArguments, newDepth)

  override def calculateApplicatives(baseArguments: Seq[Term], substitutions: Substitutions) = {
    super[Term].calculateApplicatives(baseArguments, substitutions) ++
      super[ExpressionApplication].calculateApplicatives(baseArguments, substitutions)
  }
}
