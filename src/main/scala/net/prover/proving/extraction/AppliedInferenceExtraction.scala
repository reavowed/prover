package net.prover.proving.extraction

import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.proving.derivation.SimpleDerivationStep

case class AppliedInferenceExtraction(assertionStep: Step.AssertionStep, extractionSteps: Seq[SimpleDerivationStep]) {
  def toStep: Step.AssertionOrExtraction = {
    if (extractionSteps.isEmpty) {
      assertionStep
    } else {
      Step.InferenceExtractionStep(assertionStep, extractionSteps.map(_.toProofStep))
    }
  }
  def statement: Statement = extractionSteps.lastOption.map(_.statement).getOrElse(assertionStep.statement)
}
