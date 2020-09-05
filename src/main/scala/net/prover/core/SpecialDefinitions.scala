package net.prover.core

import net.prover.core.expressions.{DefinedStatement, Statement, StatementDefinition}

case class ImplicationDefinition(statementDefinition: StatementDefinition) {
  def apply(antecedent: Statement, consequent: Statement): Statement = DefinedStatement(statementDefinition, Seq(antecedent, consequent))(Nil)
}
case class UniversalQuantificationDefinition(statementDefinition: StatementDefinition) {
  def apply(variableName: String, predicate: Statement): Statement = DefinedStatement(statementDefinition, Seq(predicate))(Seq(variableName))
}
