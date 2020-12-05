package net.prover.core

import net.prover.core.expressions.{CompoundStatement, Statement, CompoundStatementType}

case class ImplicationDefinition(statementDefinition: CompoundStatementType) {
  def apply(antecedent: Statement, consequent: Statement): Statement = CompoundStatement(statementDefinition, Seq(antecedent, consequent))(Nil)
}
case class UniversalQuantificationDefinition(statementDefinition: CompoundStatementType) {
  def apply(variableName: String, predicate: Statement): Statement = CompoundStatement(statementDefinition, Seq(predicate))(Seq(variableName))
}
