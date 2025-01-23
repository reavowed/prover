package net.prover.proving.extraction

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepLike}
import net.prover.proving.extraction.PartiallyAppliedExtraction.ChainedRewriteStep

case class PartiallyAppliedExtraction(
  mainPremise: Statement,
  extractionPremises: Seq[Statement],
  conclusion: Statement,
  extractionSteps: Seq[Step.AssertionStep],
  reversalStep: Option[Step.AssertionStep],
  leftRewrite: Option[ChainedRewriteStep],
  rightRewrite: Option[ChainedRewriteStep],
  variableTracker: VariableTracker)
{
  def getDefinition: ExtractionDefinition = {
    ExtractionDefinition(
      extractionSteps.map(_.inference),
      reversalStep.map(_.inference))
  }
  private def prependStepPremises(step: Step.AssertionStep): PartiallyAppliedExtraction = {
    copy(
      mainPremise = step.premises.head.statement,
      extractionPremises = step.premises.tail.map(_.statement) ++ extractionPremises)
  }
  private def appendStepPremisesAndConclusion(step: Step.AssertionStep): PartiallyAppliedExtraction = {
    copy(
      extractionPremises = extractionPremises ++ step.premises.tail.map(_.statement),
      conclusion = step.statement)
  }
  def prependExtractionStep(newExtractionStep: Step.AssertionStep): PartiallyAppliedExtraction = {
    prependStepPremises(newExtractionStep)
      .copy(extractionSteps = newExtractionStep +: extractionSteps)
  }
  def appendExtractionStep(
    newExtractionStep: Step.AssertionStep,
    newVariableTracker: Option[VariableTracker] = None
  ): PartiallyAppliedExtraction = {
    appendStepPremisesAndConclusion(newExtractionStep)
      .copy(
        extractionSteps = extractionSteps :+ newExtractionStep,
        variableTracker = newVariableTracker.getOrElse(variableTracker))
  }
  def prependReversal(newReversalStep: Step.AssertionStep): PartiallyAppliedExtraction = {
    prependStepPremises(newReversalStep)
      .copy(reversalStep = Some(newReversalStep))
  }
  def appendReversal(newReversalStep: Step.AssertionStep): PartiallyAppliedExtraction = {
    appendStepPremisesAndConclusion(newReversalStep)
      .copy(reversalStep = Some(newReversalStep))
  }
  def prependLeftRewrite(rewriteStep: Step.AssertionStep, chainingStep: Step.AssertionStep): PartiallyAppliedExtraction = {
    copy(
      mainPremise = chainingStep.premises.last.statement,
      extractionPremises = extractionPremises ++ rewriteStep.premises.map(_.statement),
      leftRewrite = Some(ChainedRewriteStep(rewriteStep, chainingStep)))
  }
  def appendLeftRewrite(rewriteStep: Step.AssertionStep, chainingStep: Step.AssertionStep): PartiallyAppliedExtraction = {
    copy(
      extractionPremises = extractionPremises ++ rewriteStep.premises.map(_.statement),
      conclusion = chainingStep.statement,
      leftRewrite = Some(ChainedRewriteStep(rewriteStep, chainingStep)))
  }
  def prependRightRewrite(rewriteStep: Step.AssertionStep, chainingStep: Step.AssertionStep): PartiallyAppliedExtraction = {
    copy(
      mainPremise = chainingStep.premises.head.statement,
      extractionPremises = extractionPremises ++ rewriteStep.premises.map(_.statement),
      rightRewrite = Some(ChainedRewriteStep(rewriteStep, chainingStep)))
  }
  def appendRightRewrite(rewriteStep: Step.AssertionStep, chainingStep: Step.AssertionStep): PartiallyAppliedExtraction = {
    copy(
      extractionPremises = extractionPremises ++ rewriteStep.premises.map(_.statement),
      conclusion = chainingStep.statement,
      rightRewrite = Some(ChainedRewriteStep(rewriteStep, chainingStep)))
  }

  def extractionDefinition: ExtractionDefinition = ExtractionDefinition(
    extractionSteps.map(_.inference),
    reversalStep.map(_.inference),
    leftRewrite.map(_.rewriteStep.inference),
    rightRewrite.map(_.rewriteStep.inference))

  def allSteps: Seq[Step.AssertionStep] = extractionSteps ++ reversalStep.toSeq
  def finalise(implicit provingContext: ProvingContext): AppliedExtraction = {
    AppliedExtraction(
      ExtractionApplier.groupStepsByDefinition(extractionSteps ++ reversalStep.toSeq),
      leftRewrite.toSeq.flatMap(_.substeps) ++ rightRewrite.toSeq.flatMap(_.substeps))
  }
}

object PartiallyAppliedExtraction {
  def initial(statement: Statement, variableTracker: VariableTracker): PartiallyAppliedExtraction = {
    PartiallyAppliedExtraction(statement, Nil, statement, Nil, None, None, None, variableTracker)
  }

  case class ChainedRewriteStep(rewriteStep: Step.AssertionStep, chainingStep: Step.AssertionStep) extends StepLike.Wrapper {
    override def substeps: Seq[Step.AssertionStep] = Seq(rewriteStep, chainingStep)
  }
}
