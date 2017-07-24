package net.prover.model.proof

import net.prover.model.Inference.Premise
import net.prover.model.components.Statement
import net.prover.model.proof.Proof._
import net.prover.model.{Inference, InferenceTransform, ProvenStatement}

case class ProvingContext(
  provenAssertions: Seq[ReferencedAssertion],
  provenDeductions: Seq[ReferencedDeduction],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  availableInferences: Seq[Inference],
  inferenceTransforms: Seq[InferenceTransform],
  assertionHints: Seq[AssertionHint])
{
  def addAssumption(statement: Statement, reference: Int) = {
    addAssertion(ProvenStatement.withNoConditions(statement), reference)
      .copy(assumptions = assumptions :+ statement)
  }
  def addAssertion(provenStatement: ProvenStatement, reference: Int) = {
    copy(provenAssertions = provenAssertions :+ ReferencedAssertion(provenStatement, DirectReference(reference)))
  }
  def add(referencedDeduction: ReferencedDeduction): ProvingContext = {
    copy(provenDeductions = provenDeductions :+ referencedDeduction)
  }
  def add(referencedDeductions: Seq[ReferencedDeduction]): ProvingContext = {
    copy(provenDeductions = provenDeductions ++ referencedDeductions)
  }
}
