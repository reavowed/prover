package net.prover.theorems

import net.prover.entries.{StepWithContext, TheoremWithContext}
import net.prover.model.definitions.Definitions
import net.prover.model.proof.Step
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

class IsComplete(implicit definitions: Definitions) extends RecursiveStepFinder[Boolean]()(booleanInstance.conjunction) {
  def apply(theoremWithContext: TheoremWithContext): Boolean = {
    theoremWithContext.theorem.proofs.exists(p => apply(p.steps))
  }

  override def getFromTarget(step: Step.Target): Boolean = {
    false
  }

  override def getFromAssertion(step: Step.Assertion): Boolean = {
    step.premises.forall(_.isComplete) && definitions.isInferenceComplete(step.inference)
  }

  override def getFromNaming(step: Step.Naming): Boolean = {
    apply(step.substeps) &&
      step.premises.forall(_.isComplete) &&
      definitions.isInferenceComplete(step.inference)
  }

  override def getFromDeduction(step: Step.Deduction): Boolean = {
    apply(step.substeps)
  }
  override def getFromGeneralization(step: Step.Generalization): Boolean = {
    apply(step.substeps)
  }
  override def getFromSubProof(step: Step.SubProof): Boolean = {
    apply(step.substeps)
  }
  override def getFromElided(step: Step.Elided): Boolean = {
    apply(step.substeps)
  }
  override def getFromExistingStatementExtraction(step: Step.ExistingStatementExtraction): Boolean = {
    apply(step.substeps)
  }
}

object IsComplete {
  def apply(theoremWithContext: TheoremWithContext): Boolean = {
    new IsComplete()(theoremWithContext.globalContext.definitions)(theoremWithContext)
  }
}
