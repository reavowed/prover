package net.prover.model

import net.prover._
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.expressions._

case class Substitutions(statements: Seq[Statement], terms: Seq[Term]) {
  def isEmpty: Boolean = statements.isEmpty && terms.isEmpty

  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int): Option[Substitutions] = {
    for {
      newStatements <- statements.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption
      newTerms <- terms.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption
    } yield Substitutions(newStatements, newTerms)
  }
  def replaceDefinitions(expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition]): Substitutions = {
    Substitutions(
      statements.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      terms.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }

  def serialize: String = {
    Seq(statements, terms)
      .map(x => "(" + x.map(_.serialized).mkString(", ") + ")")
      .mkString(" ")
  }

  def restrictTo(variableDefinitions: VariableDefinitions): Substitutions = {
    Substitutions(statements.take(variableDefinitions.statements.length), terms.take(variableDefinitions.terms.length))
  }
}

object Substitutions {
  val empty: Substitutions = Substitutions(Nil, Nil)

}
