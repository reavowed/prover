package net.prover.theorems

import net.prover.entries.{StepWithContext, TheoremWithContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.model.definitions.Definitions
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Monoid
import scalaz.Scalaz.*

class IsComplete(implicit definitions: Definitions) extends RecursiveStepFinder[Boolean](using booleanInstance.conjunction) {
  override def apply(theorem: Theorem): Boolean = theorem.proofs.exists(apply)

  override def apply(step: Step.TargetStep): Boolean = false
  override def apply(statement: Statement): Boolean = true
  override def apply(inference: Inference.Summary): Boolean = definitions.isInferenceComplete(inference)
  override def apply(premise: Premise): Boolean = premise.isComplete
  override def apply(substitutions: Substitutions): Boolean = true
  override def apply(deductionDefinition: DeductionDefinition): Boolean = true
  override def apply(generalizationDefinition: GeneralizationDefinition): Boolean = true
}

object IsComplete {
  def apply(theoremWithContext: TheoremWithContext): Boolean = {
    new IsComplete()(theoremWithContext.globalContext.definitions)(theoremWithContext.theorem)
  }
}
