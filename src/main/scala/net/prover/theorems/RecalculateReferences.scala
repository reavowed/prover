package net.prover.theorems

import net.prover.controllers.models.StepWithReferenceChange
import net.prover.entries.{ProofWithContext, StepWithContext, TheoremWithContext}
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Premise, Step, StepProvingContext}
import net.prover.theorems.steps.CompoundStepUpdater
import net.prover.util.FunctorTypes._
import scalaz.Scalaz._

object RecalculateReferences extends CompoundStepUpdater[WithValue[List[StepWithReferenceChange]]#Type] {
  def apply(theoremWithContext: TheoremWithContext): (Theorem, List[List[StepWithReferenceChange]]) = {
    val (updatedProofs, referenceChanges) = theoremWithContext.proofsWithContext.map(apply).split
    (theoremWithContext.theorem.copy(proofs = updatedProofs), referenceChanges.toList)
  }

  def apply(proof: ProofWithContext): (Proof, List[StepWithReferenceChange]) = {
    val (newSteps, changedSteps) = apply(proof.stepsWithContext)
    val newStepsWithTarget = if (newSteps.mapCollect(_.provenStatement).lastOption.contains(proof.theorem.conclusion)) newSteps else newSteps :+ Step.Target(proof.theorem.conclusion)
    (Proof(newStepsWithTarget), changedSteps)
  }

  override def updateAssertion(
    step: Step.Assertion,
    stepWithContext: StepWithContext
  ): (Step, List[StepWithReferenceChange]) = {
    val (newStep, innerChanges) = super.updateAssertion(step, stepWithContext)
    if (step.premises != newStep.asInstanceOf[Step.Assertion].premises) {
      (newStep, innerChanges :+ StepWithReferenceChange(newStep, stepWithContext.stepContext.stepReference.stepPath))
    } else {
      (newStep, innerChanges)
    }
  }

  override def updateNaming(
    step: Step.Naming,
    stepWithContext: StepWithContext
  ): (Step, List[StepWithReferenceChange]) = {
    val (newStep, innerChanges) = super.updateNaming(step, stepWithContext)
    if (step.premises != newStep.asInstanceOf[Step.Naming].premises) {
      (newStep, innerChanges :+ StepWithReferenceChange(newStep, stepWithContext.stepContext.stepReference.stepPath))
    } else {
      (newStep, innerChanges)
    }
  }

  override def updatePremise(
    premise: Premise,
    stepProvingContext: StepProvingContext
  ): (Premise, List[StepWithReferenceChange]) = {
    (stepProvingContext.createPremise(premise.statement), Nil)
  }
}
