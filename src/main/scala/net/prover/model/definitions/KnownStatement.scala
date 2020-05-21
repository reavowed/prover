package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.DerivationStep

case class KnownStatement(statement: Statement, derivation: Seq[DerivationStep]) {
  def addPreviousDerivation(previousDerivation: Seq[DerivationStep]): KnownStatement = {
    KnownStatement(statement, previousDerivation ++ derivation)
  }
  def extend(newStep: DerivationStep): KnownStatement = extend(Seq(newStep))
  def extend(newDerivation: Seq[DerivationStep]): KnownStatement = {
    KnownStatement.fromDerivation(derivation ++ newDerivation)
  }
}

object KnownStatement {
  def fromSingleStep(derivationStep: DerivationStep): KnownStatement = fromDerivation(Seq(derivationStep))
  def fromDerivation(derivation: Seq[DerivationStep]): KnownStatement = {
    KnownStatement(derivation.last.statement, derivation)
  }
  def deriveFromPrevious(derivationStep: DerivationStep, previousDerivation: Seq[DerivationStep]): KnownStatement = {
    KnownStatement(derivationStep.statement, previousDerivation :+ derivationStep)
  }

  implicit class SeqOps(knownStatements: Seq[KnownStatement]) {
    def deduplicate: Seq[KnownStatement] = knownStatements.distinctBy(_.statement)
  }
}
