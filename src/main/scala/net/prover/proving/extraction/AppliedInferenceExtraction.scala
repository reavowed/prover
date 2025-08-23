package net.prover.proving.extraction

import net.prover.model.*
import net.prover.model.proof.Step.AssertionStep
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.parsing.{KnownWordParser, Parser}

case class AppliedInferenceExtraction(assertionStep: Step.AssertionStep, extraction: AppliedExtraction)
  extends StepLike.Wrapper with StepLike.SimpleSingleStep
{
  def inference: Inference = assertionStep.inference
  override def substeps: Seq[StepLike] = assertionStep +: extraction.substeps
  def toProofStep: Step.AssertionOrExtraction = Step.InferenceExtractionStep.ifNecessary(this)
  override def serializedLines: Seq[String] = super.serializedLines.indentInLabelledBracesIfPresent("inferenceExtraction")
}
object AppliedInferenceExtraction {
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): KnownWordParser[AppliedInferenceExtraction] = {
    KnownWordParser("inferenceExtraction") {
      val innerStepContext = stepContext.forChild()
      (for {
        assertionStep <- AssertionStep.parser(innerStepContext, implicitly)
        extraction <- AppliedExtraction.parser(innerStepContext.addStep(assertionStep), implicitly)
      } yield AppliedInferenceExtraction(assertionStep, extraction)).inBraces
    }
  }
}
