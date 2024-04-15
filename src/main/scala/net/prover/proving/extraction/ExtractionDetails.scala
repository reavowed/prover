package net.prover.proving.extraction

import net.prover.model.expressions.Statement
import net.prover.model.proof.Step

case class ExtractionDetails(
  extractionPremises: Seq[Statement],
  conclusion: Statement,
  derivation: Seq[Step.AssertionStep],
  variableTracker: VariableTracker,
  extractionDefinition: ExtractionDefinition)
