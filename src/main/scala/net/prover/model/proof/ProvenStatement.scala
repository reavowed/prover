package net.prover.model.proof

import net.prover.model.expressions.Statement

case class ProvenStatement(statement: Statement, reference: Reference.Compoundable) {
  def insertExternalParameters(numberOfParametersToInsert: Int) = {
    ProvenStatement(statement.insertExternalParameters(numberOfParametersToInsert), reference)
  }
  def toReferencedStatement: ReferencedStatement = ReferencedStatement(statement, reference)
}
