package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.entries.TermDefinition

case class DefinedTerm(
    components: Seq[Expression],
    definition: TermDefinition)(
    val scopedBoundVariableNames: Seq[String])
  extends Term with DefinedExpression[Term]
{
  override def getMatch(other: Expression): Option[(Seq[Expression])] = other match {
    case DefinedTerm(otherComponents, `definition`) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def update(newComponents: Seq[Expression]) = {
    DefinedTerm(newComponents, definition)(scopedBoundVariableNames)
  }

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    super[Term].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
      super[DefinedExpression].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
  }
}
