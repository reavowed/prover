package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model.Substitutions

case class FunctionApplication(
    variableName: String,
    arguments: Seq[Term])
  extends ExpressionApplication[Term] with Term
{
  override def substitutionsLens = GenLens[Substitutions](_.functions)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.functions)

  def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case FunctionApplication(`variableName`, otherArguments) => Some(otherArguments)
    case _ => None
  }
  def update(newArguments: Seq[Term]) = FunctionApplication(variableName, newArguments)

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    super[Term].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
      super[ExpressionApplication].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
  }
}
