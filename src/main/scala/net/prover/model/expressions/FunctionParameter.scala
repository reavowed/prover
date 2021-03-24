package net.prover.model.expressions

import net.prover.model.{Substitutions, UsedVariables}
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.old.OldParameterInserter
import net.prover.substitutionFinding.model.PossibleSubstitutions

case class FunctionParameter(index: Int, level: Int) extends Term {
  override def structuralComplexity: Int = 1
  override def definitionalComplexity: Int = 1
  override def definitionUsages: DefinitionUsages = DefinitionUsages.empty
  override def usedVariables: UsedVariables = UsedVariables.empty

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0) = {
    if (level >= internalDepth) {
      FunctionParameter(index, level + numberOfParametersToInsert)
    } else {
      this
    }
  }
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0) = {
    if (level < internalDepth)
      Some(this)
    else if (level < internalDepth + numberOfParametersToRemove)
      None
    else
      Some(FunctionParameter(index, level - numberOfParametersToRemove))
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition]): FunctionParameter = this

  override def getPredicateForTerm(term: Term, depth: Int): Term = {
    if (term == this)
      FunctionParameter(0, depth)
    else
      this
  }

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: PossibleSubstitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(Term, PossibleSubstitutions)] = {
    (super.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth).toSet ++
      (if (level >= internalDepth + previousInternalDepth)
        // External context
        // Shifted down to cut out the shared internal context
        Seq(FunctionParameter(index, level - previousInternalDepth) -> substitutions)
      else if (level < internalDepth)
        // Internal context after the entry point to calculateApplicatives
        Seq(this -> substitutions)
      else
        // Shared internal context - must be passed in via the arguments
        Nil)
    ).iterator
  }

  override def toString: String = (0 to level).map(_ => "$").mkString("") + index
  override def serialized: String = toString
  override def serializedForHash: String = toString
}
