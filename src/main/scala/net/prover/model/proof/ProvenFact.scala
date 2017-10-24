package net.prover.model.proof

import net.prover.model.expressions.Statement

case class ProvenFact(statement: Statement, reference: Reference.Compoundable) {
  def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    ProvenFact(statement.increaseDepth(additionalDepth, insertionPoint), reference)
  }
}
