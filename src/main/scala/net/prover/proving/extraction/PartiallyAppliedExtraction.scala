package net.prover.proving.extraction

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step

case class PartiallyAppliedExtraction(
  mainPremise: Statement,
  extractionPremises: Seq[Statement],
  conclusion: Statement,
  extractionSteps: Seq[Step.AssertionStep],
  reversalStep: Option[Step.AssertionStep],
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

  def extractionDefinition: ExtractionDefinition = ExtractionDefinition(
    extractionSteps.map(_.inference),
    reversalStep.map(_.inference))

  def allSteps: Seq[Step.AssertionStep] = extractionSteps ++ reversalStep.toSeq
  def finalise(implicit provingContext: ProvingContext): AppliedExtraction = {
    ExtractionApplier.groupStepsByDefinition(allSteps)
  }
}

object PartiallyAppliedExtraction {
  def initial(statement: Statement, variableTracker: VariableTracker): PartiallyAppliedExtraction = {
    PartiallyAppliedExtraction(statement, Nil, statement, Nil, None, variableTracker)
  }
}
