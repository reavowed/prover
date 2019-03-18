package net.prover.model.proof

import net.prover.model.expressions.Statement

case class ProvenStatement(statement: Statement, reference: PreviousLineReference) {
  def insertExternalParameters(numberOfParametersToInsert: Int): ProvenStatement = {
    ProvenStatement(statement.insertExternalParameters(numberOfParametersToInsert), reference)
  }
}
