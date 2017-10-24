package net.prover.model.proof

import net.prover.model.expressions.Statement

case class ProvenStatement(statement: Statement, reference: Reference.Compoundable) {
  def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    ProvenStatement(statement.increaseDepth(additionalDepth, insertionPoint), reference)
  }
}
