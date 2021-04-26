package net.prover.model.expressions

import net.prover.model.{Substitutions, UsedVariables}
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.old.OldParameterInserter
import net.prover.substitutionFinding.model.PossibleSubstitutions

case class FunctionParameter(index: Int, level: Int) extends Term {
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

  override def toString: String = (0 to level).map(_ => "$").mkString("") + index
  override def serialized: String = toString
  override def serializedForHash: String = toString
}
