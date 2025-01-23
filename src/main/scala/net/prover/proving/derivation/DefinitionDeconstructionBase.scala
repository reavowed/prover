package net.prover.proving.derivation

import net.prover.model._
import net.prover.model.proof.{Step, StepLike}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction}

trait DefinitionDeconstructionBase extends StepLike.Wrapper {
  def deconstructionStep: Step.AssertionStep
  def additionalSteps: Seq[Step.AssertionStep]

  def inference: Inference = deconstructionStep.inference
  override def substeps: Seq[StepLike] = deconstructionStep +: additionalSteps
  def toProofStep: Step.AssertionOrExtraction = {
    Step.InferenceExtractionStep.ifNecessary(AppliedInferenceExtraction(
      deconstructionStep,
      AppliedExtraction.fromSimpleExtraction(additionalSteps)))
  }
  override def serializedLines: Seq[String] = {
    super.serializedLines.indentInLabelledBracesIfPresent(DefinitionDeconstructionBase.label)
  }
}

object DefinitionDeconstructionBase {
  val label = "deconstruction"
}
