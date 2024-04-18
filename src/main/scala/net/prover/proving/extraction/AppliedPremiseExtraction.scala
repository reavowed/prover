package net.prover.proving.extraction

import net.prover.model.proof.Step
import net.prover.proving.derivation.SimpleDerivationStep

case class AppliedPremiseExtraction(extractionSteps: Seq[SimpleDerivationStep]) {
  def toStep: Option[Step] = extractionSteps match {
    case Nil => None
    case singleStep +: Nil => Some(singleStep.toProofStep)
    case steps => Some(Step.ExistingStatementExtractionStep(steps.map(_.toProofStep)))
  }
}
