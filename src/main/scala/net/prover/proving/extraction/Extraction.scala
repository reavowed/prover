package net.prover.proving.extraction

import net.prover.model.{Inference, VariableDefinitions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.StepContext

sealed trait Extraction {
  def premises: Seq[Statement]
  def conclusion: Statement
  def variableDefinitions: VariableDefinitions
  def extractionDefinition: ExtractionDefinition
  def additionalVariableNames: Seq[String]
}

case class InferenceExtraction(inference: Inference.Summary, extractionDetails: PartiallyAppliedExtraction) extends Extraction {
  def premises: Seq[Statement] = inference.premises ++ extractionDetails.extractionPremises
  def conclusion: Statement = extractionDetails.conclusion
  def variableDefinitions: VariableDefinitions = inference.variableDefinitions.addSimpleTermVariableNames(extractionDetails.variableTracker.additionalVariableNames)
  def extractionDefinition: ExtractionDefinition = extractionDetails.extractionDefinition
  def additionalVariableNames: Seq[String] = extractionDetails.variableTracker.additionalVariableNames
  def derivedSummary: Inference.Summary = Inference.Summary(inference.name, Inference.calculateHash(premises, conclusion), variableDefinitions, premises, conclusion)
}

case class PremiseExtraction(extractionDetails: PartiallyAppliedExtraction, stepContext: StepContext) extends Extraction {
  def premises: Seq[Statement] = extractionDetails.extractionPremises
  def conclusion: Statement = extractionDetails.conclusion
  def variableDefinitions: VariableDefinitions = stepContext.variableDefinitions.addSimpleTermVariableNames(extractionDetails.variableTracker.additionalVariableNames)
  def extractionDefinition: ExtractionDefinition = extractionDetails.extractionDefinition
  def additionalVariableNames: Seq[String] = extractionDetails.variableTracker.additionalVariableNames
}
