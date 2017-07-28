package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.Inference.Premise
import net.prover.model.components.Statement
import net.prover.model.proof.Proof._

case class ProvingContext(
  provenAssertions: Seq[ReferencedAssertion],
  provenDeductions: Seq[ReferencedDeduction],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  availableInferences: Seq[Inference],
  assertionHints: Seq[AssertionHint])
{
  def addAssumption(statement: Statement, reference: Int) = {
    addAssertion(statement, reference)
      .copy(assumptions = assumptions :+ statement)
  }
  def addAssertion(statement: Statement, reference: Int) = {
    copy(provenAssertions = provenAssertions :+ ReferencedAssertion(statement, DirectReference(reference)))
  }
  def add(referencedDeduction: ReferencedDeduction): ProvingContext = {
    copy(provenDeductions = provenDeductions :+ referencedDeduction)
  }
  def add(referencedDeductions: Seq[ReferencedDeduction]): ProvingContext = {
    copy(provenDeductions = provenDeductions ++ referencedDeductions)
  }
}
