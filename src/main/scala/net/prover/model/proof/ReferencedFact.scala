package net.prover.model.proof

import net.prover.model.expressions.Statement

case class ReferencedFact(statement: Statement, reference: Reference.ToFact) {
  def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    ReferencedFact(statement.increaseDepth(additionalDepth, insertionPoint), reference)
  }
}
