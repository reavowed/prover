package net.prover.model.definitions

import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.proving.extraction.ExtractionApplier
import net.prover.proving.extraction.ExtractionCalculator.InferenceExtraction

case class Fact(inferenceExtraction: InferenceExtraction) {
  def statement: Statement = inferenceExtraction.conclusion
  def inference: Inference = inferenceExtraction.inference

  def toStep(implicit provingContext: ProvingContext): Step.AssertionOrExtraction = {
    val assertionStep = Step.Assertion(
      inferenceExtraction.inference.conclusion,
      inferenceExtraction.inference.summary,
      Nil,
      Substitutions.empty)
    ExtractionApplier.createDerivationForInferenceExtraction(
      assertionStep,
      inferenceExtraction.innerExtraction.derivation)
  }
  def toKnownStatement(implicit provingContext: ProvingContext): KnownStatement = {
    KnownStatement.fromSingleStep(toStep)
  }
}
