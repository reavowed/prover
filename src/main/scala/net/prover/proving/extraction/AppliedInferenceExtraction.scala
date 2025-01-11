package net.prover.proving.extraction

import net.prover.model.proof.{Step, StepLike}

case class AppliedInferenceExtraction(assertionStep: Step.AssertionStep, extraction: AppliedExtraction) extends StepLike.Wrapper {
  override def substeps: Seq[StepLike] = assertionStep +: extraction.extractionSteps
  def toStep: Step.AssertionOrExtraction = {
    if (extraction.extractionSteps.isEmpty) {
      assertionStep
    } else {
      Step.InferenceExtractionStep(assertionStep, extraction.extractionSteps.map(_.toProofStep))
    }
  }
}
