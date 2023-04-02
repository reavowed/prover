package net.prover.entries

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Premise, PremiseReference, StepReference}
import net.prover.model.{AvailableEntries, ExpressionParsingContext, ProvingContext}

case class ProofWithContext(proof: Proof, proofIndex: Int, theoremWithContext: TheoremWithContext) {
  def theorem: Theorem = theoremWithContext.theorem
  implicit def expressionParsingContext: ExpressionParsingContext = theoremWithContext.expressionParsingContext
  implicit def availableEntries: AvailableEntries = theoremWithContext.availableEntries
  implicit def provingContext: ProvingContext = theoremWithContext.provingContext
  def globalContext: GlobalContext = theoremWithContext.globalContext

  def stepsWithContext: StepsWithContext = {
    StepsWithContext(
      proof.steps,
      theorem.initialStepContext,
      this)
  }
}
