package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}

case class Fact(inferenceExtraction: InferenceExtraction) {
  def statement: Statement = inferenceExtraction.conclusion
  def inference: Inference = inferenceExtraction.inference

  def derivation(implicit provingContext: ProvingContext): Seq[Step.AssertionOrExtraction] = {
    ExtractionApplier.groupStepsByDefinition(inferenceExtraction.extractionDetails.derivation)
  }

  def toStep(implicit provingContext: ProvingContext): Step.AssertionOrExtraction = {
    val baseAssertion = Step.AssertionStep(
      inferenceExtraction.inference.conclusion,
      inferenceExtraction.inference.summary,
      Nil,
      Substitutions.empty)
    baseAssertion.addExtractionSteps(derivation)
  }
  def toKnownStatement(implicit provingContext: ProvingContext): KnownStatement = {
    KnownStatement.fromSingleStep(toStep)
  }
}
