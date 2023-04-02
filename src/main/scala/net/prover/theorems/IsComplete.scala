package net.prover.theorems

import net.prover.entries.{StepWithContext, TheoremWithContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.model.definitions.{DeductionDefinition, Definitions, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

class IsComplete(implicit definitions: Definitions) extends RecursiveStepFinder[Boolean]()(booleanInstance.conjunction) {
  def apply(theoremWithContext: TheoremWithContext): Boolean = {
    theoremWithContext.theorem.proofs.exists(p => apply(p.steps))
  }

  override def apply(step: Step.Target): Boolean = false

  override def apply(statement: Statement): Boolean = true
  override def apply(inference: Inference.Summary): Boolean = definitions.isInferenceComplete(inference)
  override def apply(premise: Premise): Boolean = premise.isComplete
  override def apply(substitutions: Substitutions): Boolean = true
  override def apply(deductionDefinition: DeductionDefinition): Boolean = true
  override def apply(generalizationDefinition: GeneralizationDefinition): Boolean = true
}

object IsComplete {
  def apply(theoremWithContext: TheoremWithContext): Boolean = {
    new IsComplete()(theoremWithContext.globalContext.definitions)(theoremWithContext)
  }
}
