package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model.Substitutions

case class PredicateApplication(
    variableName: String,
    arguments: Seq[Term])
  extends ExpressionApplication[Statement]
    with Statement
{
  override def substitutionsLens = GenLens[Substitutions](_.predicates)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.predicates)

  def update(newArguments: Seq[Term]) = PredicateApplication(variableName, newArguments)
}
