package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.entries.TermDefinition

case class DefinedTerm(
    components: Seq[Expression],
    definition: TermDefinition,
    depth: Int)(
    val scopedBoundVariableNames: Seq[String])
  extends Term with DefinedExpression[Term]
{
  override def getMatch(other: Expression): Option[(Seq[Expression])] = other match {
    case DefinedTerm(otherComponents, `definition`, _) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def update(newComponents: Seq[Expression], newDepth: Int) = {
    DefinedTerm(newComponents, definition, newDepth)(scopedBoundVariableNames)
  }

  override def calculateApplicatives(baseArguments: ArgumentList, substitutions: Substitutions) = {
    super[Term].calculateApplicatives(baseArguments, substitutions) ++
      super[DefinedExpression].calculateApplicatives(baseArguments, substitutions)
  }
}
