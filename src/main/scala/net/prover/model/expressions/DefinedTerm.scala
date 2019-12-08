package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.entries.{ExpressionDefinition, TermDefinition}

case class DefinedTerm(
    components: Seq[Expression],
    definition: TermDefinition)(
    val scopedBoundVariableNames: Seq[String])
  extends Term with DefinedExpression[Term]
{
  override def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case DefinedTerm(otherComponents, `definition`) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def updateComponents(newComponents: Seq[Expression]): DefinedTerm = {
    DefinedTerm(newComponents, definition)(scopedBoundVariableNames)
  }
  override def updateBoundVariableNames(newBoundVariableNames: Seq[String]): DefinedTerm = {
    DefinedTerm(components, definition)(newBoundVariableNames)
  }
  override def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): DefinedTerm = {
    DefinedTerm(
      components.map(_.replaceDefinition(oldDefinition, newDefinition)),
      if (definition == oldDefinition) newDefinition.asInstanceOf[TermDefinition] else definition
    )(scopedBoundVariableNames)
  }

  override def getTerms(depth: Int): Seq[(Term, Term, Seq[Int])] = super[Term].getTerms(depth) ++ super[DefinedExpression].getTerms(depth)
  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(Term, Substitutions.Possible)] = {
    super[Term].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth) ++
      super[DefinedExpression].calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
  }
}
