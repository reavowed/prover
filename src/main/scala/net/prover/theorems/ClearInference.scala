package net.prover.theorems

import net.prover.books.management.BookStateManager
import net.prover.controllers._
import net.prover.entries.StepWithContext
import net.prover.model.Inference
import net.prover.model.proof.{Premise, Step}
import net.prover.refactoring.UpdateTheorems
import scalaz.Id.Id

case class ClearInference(inferenceToClear: Inference) extends CompoundTheoremUpdater[Id] {
  override def updateNaming(
    step: Step.Naming,
    stepWithContext: StepWithContext
  ): Step = {
    if (inferenceToClear == step.inference) {
      Step.Target(step.statement)
    } else {
      super.updateNaming(step, stepWithContext)
    }
  }

  override def updateElided(
    step: Step.Elided,
    stepWithContext: StepWithContext
  ): Step = {
    if (step.highlightedInference.contains(inferenceToClear)) {
      Step.Target(step.statement)
    } else {
      super.updateElided(step, stepWithContext)
    }
  }

  override def updateAssertion(
    step: Step.Assertion,
    stepWithContext: StepWithContext
  ): Step = {
    if (inferenceToClear == step.inference) {
      Step.Target(step.statement)
    } else {
      super.updateAssertion(step, stepWithContext)
    }
  }

  override def updatePremise(
    premise: Premise,
    stepWithContext: StepWithContext
  ): Premise = {
    stepWithContext.stepProvingContext.allPremises
      .filter(p => !p.referencedInferences.contains(inferenceToClear))
      .find(_.statement == premise.statement)
      .orBadRequest("Could not find premise not using inference").get
  }
}

object ClearInference {
  def apply(
    inferenceId: String)(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    UpdateTheorems(globalContext => {
      val inference = globalContext.definitions.allInferences.find(_.id == inferenceId).get
      ClearInference(inference)(_)
    })
  }
}
