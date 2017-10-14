package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model.Substitutions

case class FunctionApplication(
    variableName: String,
    arguments: ArgumentList)
  extends ExpressionApplication[Term] with Term
{
  override def substitutionsLens = GenLens[Substitutions](_.functions)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.functions)

  def update(newArguments: ArgumentList) = FunctionApplication(variableName, newArguments)

  override def calculateApplicatives(baseArguments: ArgumentList, substitutions: Substitutions) = {
    super[Term].calculateApplicatives(baseArguments, substitutions) ++
      super[ExpressionApplication].calculateApplicatives(baseArguments, substitutions)
  }
}
