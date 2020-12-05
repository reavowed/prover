package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.definitions.{CompoundExpressionDefinition, CompoundTermDefinition}

case class DefinedTerm(
    components: Seq[Expression],
    definition: CompoundTermDefinition)(
    val boundVariableNames: Seq[String])
  extends Term with DefinedExpression[Term]
{
  override def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case DefinedTerm(otherComponents, `definition`) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def updateComponents(newComponents: Seq[Expression]): DefinedTerm = {
    DefinedTerm(newComponents, definition)(boundVariableNames)
  }
  override def updateBoundVariableNames(newBoundVariableNames: Seq[String]): DefinedTerm = {
    DefinedTerm(components, definition)(newBoundVariableNames)
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition]): DefinedTerm = {
    DefinedTerm(
      components.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      expressionDefinitionReplacements(definition).asInstanceOf[CompoundTermDefinition]
    )(boundVariableNames)
  }

  override def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, Term, Int, Seq[Int])] = {
    super[Term].getTerms(internalDepth, externalDepth) ++
      super[DefinedExpression].getTerms(internalDepth, externalDepth)
  }
  override def getPredicateForTerm(term: Term, depth: Int): Term = {
    if (term == this)
      FunctionParameter(0, depth)
    else
      super.getPredicateForTerm(term, depth)
  }

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
