package net.prover.theorems

import net.prover.controllers.models.StepWithReferenceChange
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.Statement
import net.prover.model._
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext}
import net.prover.theorems.steps.CompoundStepUpdater
import net.prover.util.FunctorTypes._
import scalaz.Scalaz._

object RecalculateReferences extends CompoundStepUpdater[ProvingContext, WithValue[List[StepWithReferenceChange]]#Type] {
  def apply(theorem: Theorem, provingContext: ProvingContext): (Theorem, List[List[StepWithReferenceChange]]) = {
    val (updatedProofs, referenceChanges) = theorem.proofs.map(apply(_, theorem.initialStepContext, theorem.conclusion, provingContext)).split
    (theorem.copy(proofs = updatedProofs), referenceChanges.toList)
  }

  def apply(proof: Proof, initialStepContext: StepContext, expectedConclusion: Statement, provingContext: ProvingContext): (Proof, List[StepWithReferenceChange]) = {
    val (newSteps, changedSteps) = apply(proof.steps.toList, initialStepContext, provingContext)
    val newStepsWithTarget = if (newSteps.mapCollect(_.provenStatement).lastOption.contains(expectedConclusion)) newSteps else newSteps :+ Step.Target(expectedConclusion)
    (Proof(newStepsWithTarget), changedSteps)
  }

  override def updateAssertion(
    step: Step.Assertion,
    stepContext: StepContext,
    provingContext: ProvingContext
  ): (Step, List[StepWithReferenceChange]) = {
    val (newStep, innerChanges) = super.updateAssertion(step, stepContext, provingContext)
    if (step.premises != newStep.asInstanceOf[Step.Assertion].premises) {
      (newStep, innerChanges :+ StepWithReferenceChange(newStep, stepContext.stepReference.stepPath))
    } else {
      (newStep, innerChanges)
    }
  }

  override def updateNaming(
    step: Step.Naming,
    stepContext: StepContext,
    provingContext: ProvingContext
  ): (Step, List[StepWithReferenceChange]) = {
    val (newStep, innerChanges) = super.updateNaming(step, stepContext, provingContext)
    if (step.premises != newStep.asInstanceOf[Step.Naming].premises) {
      (newStep, innerChanges :+ StepWithReferenceChange(newStep, stepContext.stepReference.stepPath))
    } else {
      (newStep, innerChanges)
    }
  }

  override def updateStatement(
    statement: Statement,
    stepContext: StepContext,
    provingContext: ProvingContext
  ): (Statement, List[StepWithReferenceChange]) = (statement, Nil)

  override def updateInference(
    inference: Inference.Summary,
    stepContext: StepContext,
    provingContext: ProvingContext
  ): (Inference.Summary, List[StepWithReferenceChange]) = (inference, Nil)

  override def updatePremise(
    premise: Premise,
    stepContext: StepContext,
    provingContext: ProvingContext
  ): (Premise, List[StepWithReferenceChange]) = {
    (StepProvingContext(stepContext, provingContext).createPremise(premise.statement), Nil)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext,
    provingContext: ProvingContext
  ): (Substitutions, List[StepWithReferenceChange]) = (substitutions, Nil)
}
