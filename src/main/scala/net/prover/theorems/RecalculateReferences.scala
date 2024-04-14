package net.prover.theorems

import net.prover.books.management.BookStateManager
import net.prover.controllers._
import net.prover.controllers.models.StepWithReferenceChange
import net.prover.entries.{ProofWithContext, StepWithContext, TheoremWithContext}
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Premise, Step}
import net.prover.refactoring.UpdateTheorems
import net.prover.theorems.steps.CompoundStepUpdater
import net.prover.util.FunctorTypes._
import scalaz.Scalaz._

import scala.util.Try

object RecalculateReferences extends CompoundStepUpdater[FWithValue[Try, List[StepWithReferenceChange]]#Type] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => theorem => apply(theorem).get._1)
  }

  def apply(theoremWithContext: TheoremWithContext): Try[(Theorem, List[List[StepWithReferenceChange]])] = {
    for {
      (updatedProofs, referenceChanges) <- theoremWithContext.proofsWithContext.toList.map(apply).sequence.map(_.split)
    } yield (theoremWithContext.theorem.copy(proofs = updatedProofs), referenceChanges.toList)
  }

  def apply(proof: ProofWithContext): Try[(Proof, List[StepWithReferenceChange])] = {
    for {
      (newSteps, changedSteps) <- apply(proof.stepsWithContext)
      _ <- newSteps.map(_.statement).lastOption.contains(proof.theorem.conclusion).orBadRequest("Theorem no longer proves expected result")
    } yield (Proof(newSteps), changedSteps)
  }

  override def updateAssertion(
    step: Step.AssertionStep,
    stepWithContext: StepWithContext
  ): Try[(Step, List[StepWithReferenceChange])] = {
    super.updateAssertion(step, stepWithContext) map {
      case (newStep, innerChanges) =>
        if (step.premises != newStep.asInstanceOf[Step.AssertionStep].premises) {
          (newStep, innerChanges :+ StepWithReferenceChange(newStep, stepWithContext.stepContext.stepReference.stepPath))
        } else {
          (newStep, innerChanges)
        }
    }
  }

  override def updateNaming(
    step: Step.NamingStep,
    stepWithContext: StepWithContext
  ): Try[(Step, List[StepWithReferenceChange])] = {
    super.updateNaming(step, stepWithContext) map {
      case (newStep, innerChanges) =>
        if (step.premises != newStep.asInstanceOf[Step.NamingStep].premises) {
          (newStep, innerChanges :+ StepWithReferenceChange(newStep, stepWithContext.stepContext.stepReference.stepPath))
        } else {
          (newStep, innerChanges)
        }
    }
  }

  override def updatePremise(
    premise: Premise,
    stepWithContext: StepWithContext
  ): Try[(Premise, List[StepWithReferenceChange])] = {
    stepWithContext.stepProvingContext.findPremise(premise.statement)
      .orBadRequest("Premise reference broken")
      .map(_ -> Nil)
  }
}
