package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{AppliedInferenceExtraction, ExtractionApplier, InferenceExtraction}

case class Fact(inferenceExtraction: InferenceExtraction) {
  def statement: Statement = inferenceExtraction.conclusion
  def inference: Inference = inferenceExtraction.inference

  def derivation(implicit provingContext: ProvingContext): SimpleDerivation = {
    SimpleDerivation(ExtractionApplier.groupStepsByDefinition(inferenceExtraction.extractionDetails.derivation))
  }
  def toSingleDerivationStep(implicit provingContext: ProvingContext): SimpleDerivationStep = {
    val baseAssertion = Step.AssertionStep(
      inferenceExtraction.inference.conclusion,
      inferenceExtraction.inference.summary,
      Nil,
      Substitutions.empty)
    SimpleDerivationStep.Simplification(AppliedInferenceExtraction(baseAssertion, derivation.steps))
  }
  def toProofStep(implicit provingContext: ProvingContext): Step = {
    toSingleDerivationStep.toProofStep
  }

  def toKnownStatement(implicit provingContext: ProvingContext): KnownStatement = {
    KnownStatement.fromSingleStep(toSingleDerivationStep)
  }
}
