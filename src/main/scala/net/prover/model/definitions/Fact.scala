package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction, ExtractionApplier, InferenceExtraction}

case class Fact(inferenceExtraction: InferenceExtraction) {
  def statement: Statement = inferenceExtraction.conclusion
  def inference: Inference = inferenceExtraction.inference

  def extraction(implicit provingContext: ProvingContext): AppliedExtraction = {
    inferenceExtraction.extractionDetails.finalise
  }
  def toExtraction(implicit provingContext: ProvingContext): AppliedInferenceExtraction = {
    val baseAssertion = Step.AssertionStep(
      inferenceExtraction.inference.conclusion,
      inferenceExtraction.inference.summary,
      Nil,
      Substitutions.empty)
    AppliedInferenceExtraction(baseAssertion, extraction)
  }
  def toProofStep(implicit provingContext: ProvingContext): Step = {
    Step.InferenceExtractionStep(toExtraction)
  }

  def toKnownStatement(implicit provingContext: ProvingContext): KnownStatement = {
    KnownStatement.fromSingleStep(SimpleDerivationStep(toExtraction))
  }
}
