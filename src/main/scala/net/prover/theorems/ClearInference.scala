package net.prover.theorems

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext}
import net.prover.model.proof.Premise.Simplification
import net.prover.model.{Inference, Substitutions}
import net.prover.util.FunctorTypes.Identity

object ClearInference extends CompoundTheoremUpdater[Inference, Identity] {

  override def updateNaming(
    step: Step.Naming,
    stepContext: StepContext,
    inferenceToClear: Inference
  ): Step = {
    if (inferenceToClear == step.inference) {
      Step.Target(step.statement)
    } else {
      super.updateNaming(step, stepContext, inferenceToClear)
    }
  }

  override def updateElided(
    step: Step.Elided,
    stepContext: StepContext,
    inferenceToClear: Inference
  ): Step = {
    if (step.highlightedInference.contains(inferenceToClear)) {
      Step.Target(step.provenStatement.get)
    } else {
      super.updateElided(step, stepContext, inferenceToClear)
    }
  }

  override def updateAssertion(
    step: Step.Assertion,
    stepContext: StepContext,
    inferenceToClear: Inference
  ): Step = {
    if (inferenceToClear == step.inference) {
      Step.Target(step.statement)
    } else {
      super.updateAssertion(step, stepContext, inferenceToClear)
    }
  }

  override def updateStatement(
    statement: Statement,
    stepContext: StepContext,
    inferenceToClear: Inference
  ): Statement = {
    statement
  }

  override def updateInference(
    inference: Inference.Summary,
    stepContext: StepContext,
    inferenceToClear: Inference
  ): Inference.Summary = {
    inference
  }

  override def updatePremise(
    premise: Premise,
    stepContext: StepContext,
    inferenceToClear: Inference
  ): Premise = {
    premise match {
      case Simplification(statement, _, `inferenceToClear`, _, _) =>
        Premise.Pending(statement)
      case Simplification(statement, inner, _, _, _) if updatePremise(inner, stepContext, inferenceToClear) != inner =>
        Premise.Pending(statement)
      case premise =>
        premise
    }
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext,
    inferenceToClear: Inference
  ): Substitutions = {
    substitutions
  }
}
