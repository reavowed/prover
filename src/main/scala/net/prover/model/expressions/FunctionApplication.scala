package net.prover.model.expressions

import monocle.Lens
import monocle.macros.GenLens
import net.prover.model.Substitutions

case class FunctionApplication(
    variableName: String,
    arguments: Seq[Term])
  extends ExpressionApplication[Term] with Term
{
  override def substitutionsLens = GenLens[Substitutions](_.functions)
  override def possibleSubstitutionsLens = GenLens[Substitutions.Possible](_.functions)
  override def possibleSubstitutionsApplicationsLens = GenLens[Substitutions.Possible](_.functionApplications)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.functions)

  def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case FunctionApplication(`variableName`, otherArguments) => Some(otherArguments)
    case _ => None
  }
  def update(newArguments: Seq[Term]) = FunctionApplication(variableName, newArguments)

  override def getTerms(depth: Int): Seq[(Term, Term)] = super[Term].getTerms(depth) ++ super[ExpressionApplication].getTerms(depth)
  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    super[Term].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
      super[ExpressionApplication].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
  }
}
