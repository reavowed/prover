package net.prover.proving.structure.definitions

import net.prover.model.definitions.StatementDefinition
import net.prover.model.expressions.Statement

case class ConjunctionDefinition(statementDefinition: StatementDefinition) extends SpecialStatementDefinition.BinaryConnective {
  def all(statements: Statement*): Statement = statements.reduceRight(apply)
}
