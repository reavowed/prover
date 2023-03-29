package net.prover.entries

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.StepReference
import net.prover.model.{EntryContext, ExpressionParsingContext, ProvingContext}

case class ProofWithContext(proof: Proof, proofIndex: Int, theoremWithContext: TheoremWithContext) {
  def theorem: Theorem = theoremWithContext.theorem
  implicit def expressionParsingContext: ExpressionParsingContext = theoremWithContext.expressionParsingContext
  implicit def entryContext: EntryContext = theoremWithContext.entryContext
  implicit def provingContext: ProvingContext = theoremWithContext.provingContext

  def stepsWithContext: StepsWithContext = {
    StepsWithContext(proof.steps, StepReference(Nil), theorem.initialStepContext, this)
  }
}
