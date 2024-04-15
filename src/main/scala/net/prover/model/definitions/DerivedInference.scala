package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.{Inference, VariableDefinitions}
import net.prover.proving.extraction.{ExtractionDefinition, InferenceExtraction}

trait DerivedInference {
  def inferenceExtraction: InferenceExtraction

  def premises: Seq[Statement] = inferenceExtraction.premises
  def conclusion: Statement = inferenceExtraction.conclusion
  def variableDefinitions: VariableDefinitions = inferenceExtraction.variableDefinitions
  def extractionDefinition: ExtractionDefinition = inferenceExtraction.extractionDefinition

  def baseInference: Inference.Summary = inferenceExtraction.inference
  def derivedSummary: Inference.Summary = inferenceExtraction.derivedSummary
}
