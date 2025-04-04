package net.prover.model.definitions

import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.parsing.{KnownWordParser, Parser}
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{AppliedInferenceExtraction, PremiseExtraction}

case class KnownStatement(override val statement: Statement, derivation: SimpleDerivation) extends StepLike.Wrapper {
  override def substeps: Seq[StepLike] = Seq(derivation)
  def extend(appliedInferenceExtraction: AppliedInferenceExtraction): KnownStatement = {
    extend(Seq(SimpleDerivationStep(appliedInferenceExtraction)))
  }
  def extend(newDerivation: SimpleDerivation): KnownStatement = {
    extend(newDerivation.steps)
  }
  def extend(newSteps: Seq[SimpleDerivationStep]): KnownStatement = {
    KnownStatement.fromDerivation(derivation ++ newSteps)
  }
  def toProofSteps: Seq[Step.AssertionOrExtraction] = derivation.toProofSteps
  override def serializedLines: Seq[String] = {
    if (derivation.nonEmpty)
      derivation.serializedLines.indentInLabelledBracesIfPresent("knownDerived " + statement.serialized)
    else
      Seq("knownDirect " + statement.serialized)
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
    val appliedExtraction = premiseExtraction.extractionDetails.finalise
    KnownStatement(premiseExtraction.conclusion, SimpleDerivation.fromExtraction(appliedExtraction))
  }

  implicit class SeqOps(knownStatements: Seq[KnownStatement]) {
    def deduplicate: Seq[KnownStatement] = knownStatements.distinctBy(_.statement)
  }

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): KnownWordParser[KnownStatement] = {
    val directParser = KnownWordParser("knownDirect") {
      Statement.parser.map(KnownStatement(_, SimpleDerivation.empty))
    }
    val derivedParser = KnownWordParser("knownDerived") {
      for {
        statement <- Statement.parser
        derivation <- SimpleDerivation.parser.inBraces
      } yield KnownStatement(statement, derivation)
    }
    KnownWordParser.select(Seq(directParser, derivedParser))
  }
  def listParser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[(Seq[KnownStatement], StepContext)] = {
    StepLike.listParser[KnownStatement](parser(_, implicitly), (sc, ks) => sc.addSteps(ks.derivation.toProofSteps))
  }
}
