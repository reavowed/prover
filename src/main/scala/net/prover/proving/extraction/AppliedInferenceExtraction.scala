package net.prover.proving.extraction

import net.prover.model.proof.Step.AssertionStep
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.model.{Inference, Parser, ProvingContext}

case class AppliedInferenceExtraction(assertionStep: Step.AssertionStep, extraction: AppliedExtraction) extends StepLike.Wrapper {
  def inference: Inference = assertionStep.inference
  override def substeps: Seq[StepLike] = assertionStep +: extraction.substeps
  def toStep: Step.AssertionOrExtraction = Step.InferenceExtractionStep.ifNecessary(this)
  override def serializedLines: Seq[String] = Seq(assertionStep, extraction).flatMap(_.serializedLines)
}
object AppliedInferenceExtraction {
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[AppliedInferenceExtraction] = {
    val innerStepContext = stepContext.forChild()
    (for {
      _ <- Parser.requiredWord(AssertionStep.label)
      assertionStep <- AssertionStep.parser(innerStepContext, implicitly)
      extraction <- AppliedExtraction.parser(innerStepContext.addStep(assertionStep), implicitly)
    } yield AppliedInferenceExtraction(assertionStep, extraction)).inBraces
  }
}
