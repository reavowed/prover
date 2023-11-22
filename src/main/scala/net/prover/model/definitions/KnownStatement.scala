package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.{DerivationStep, Step}

case class KnownStatement(statement: Statement, derivation: Seq[Step.AssertionOrExtraction]) {
  def extend(newStep: Step.AssertionOrExtraction): KnownStatement = extend(Seq(newStep))
  def extend(newDerivation: Seq[Step.AssertionOrExtraction]): KnownStatement = {
    KnownStatement.fromDerivation(derivation ++ newDerivation)
  }
}

object KnownStatement {
  def fromSingleStep(derivationStep: Step.AssertionOrExtraction): KnownStatement = fromDerivation(Seq(derivationStep))
  def fromDerivation(derivation: Seq[Step.AssertionOrExtraction]): KnownStatement = {
    KnownStatement(derivation.last.statement, derivation)
  }

  implicit class SeqOps(knownStatements: Seq[KnownStatement]) {
    def deduplicate: Seq[KnownStatement] = knownStatements.distinctBy(_.statement)
  }
}
