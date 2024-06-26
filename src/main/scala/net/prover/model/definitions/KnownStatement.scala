package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{ExtractionApplier, PremiseExtraction}

case class KnownStatement(statement: Statement, derivation: SimpleDerivation) {
  def extend(newStep: SimpleDerivationStep): KnownStatement = extend(Seq(newStep))
  def extend(newDerivation: SimpleDerivation): KnownStatement = {
    extend(newDerivation.steps)
  }
  def extend(newSteps: Seq[SimpleDerivationStep]): KnownStatement = {
    KnownStatement.fromDerivation(derivation ++ newSteps)
  }
}

object KnownStatement {
  def fromSingleStep(derivationStep: SimpleDerivationStep): KnownStatement = fromDerivationSteps(Seq(derivationStep))
  def fromDerivationSteps(derivationSteps: Seq[SimpleDerivationStep]): KnownStatement = {
    fromDerivation(SimpleDerivation(derivationSteps))
  }
  def fromDerivation(derivation: SimpleDerivation): KnownStatement = {
    KnownStatement(derivation.statement, derivation)
  }
  def fromExtraction(premiseExtraction: PremiseExtraction)(implicit provingContext: ProvingContext) : KnownStatement = {
    KnownStatement(premiseExtraction.conclusion, SimpleDerivation(ExtractionApplier.groupStepsByDefinition(premiseExtraction.extractionDetails.derivation)))
  }

  implicit class SeqOps(knownStatements: Seq[KnownStatement]) {
    def deduplicate: Seq[KnownStatement] = knownStatements.distinctBy(_.statement)
  }
}
