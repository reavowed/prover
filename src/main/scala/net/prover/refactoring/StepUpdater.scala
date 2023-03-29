package net.prover.refactoring

import net.prover.entries.{ProofWithContext, StepWithContext, StepsWithContext, TheoremWithContext}
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.theorems.RecalculateReferences

import scala.util.{Success, Try}

trait StepUpdater extends TheoremUpdater {
  override def updateTheorem(theoremWithContext: TheoremWithContext): Try[Theorem] = {
    import theoremWithContext._
    proofsWithContext
      .map(updateProof).traverseTry
      .map(updatedProofs => RecalculateReferences(theorem.copy(proofs = updatedProofs), theoremWithContext.provingContext)._1)
  }

  def updateProof(proofWithContext: ProofWithContext): Try[Proof] = {
    updateSteps(proofWithContext.stepsWithContext).map(newSteps => proofWithContext.proof.copy(steps = newSteps))
  }

  def updateSteps(stepsWithContext: StepsWithContext): Try[Seq[Step]] = {
    import stepsWithContext._
    steps.mapReduceTryWithPrevious[Step]{ case (before, step) => updateStep(stepsWithContext.atChild(before, step)) }
  }

  def updateStep(stepWithContext: StepWithContext): Try[Step] = {
    stepWithContext.step match {
      case target: Step.Target =>
        updateTarget(target, stepWithContext).getOrElse(updateStepWithoutSubsteps(target, stepWithContext))
      case assertion: Step.Assertion =>
        updateAssertion(assertion, stepWithContext).getOrElse(updateStepWithoutSubsteps(assertion, stepWithContext))
      case deduction: Step.Deduction =>
        updateDeduction(deduction, stepWithContext).getOrElse(updateStepWithSubsteps(deduction, stepWithContext))
      case generalization: Step.Generalization =>
        updateGeneralization(generalization, stepWithContext).getOrElse(updateStepWithSubsteps(generalization, stepWithContext))
      case naming: Step.Naming =>
        updateNaming(naming, stepWithContext).getOrElse(updateStepWithSubsteps(naming, stepWithContext))
      case subProof: Step.SubProof =>
        updateSubProof(subProof, stepWithContext).getOrElse(updateStepWithSubsteps(subProof, stepWithContext))
      case elided: Step.Elided =>
        updateElided(elided, stepWithContext).getOrElse(updateStepWithSubsteps(elided, stepWithContext))
      case existingStatementExtraction: Step.ExistingStatementExtraction =>
        updateExistingStatementExtraction(existingStatementExtraction, stepWithContext).getOrElse(updateStepWithSubsteps(existingStatementExtraction, stepWithContext))
    }
  }

  def updateTarget(step: Step.Target, stepWithContext: StepWithContext): Option[Try[Step]] = None
  def updateAssertion(step: Step.Assertion, stepWithContext: StepWithContext): Option[Try[Step]] = None
  def updateDeduction(step: Step.Deduction, stepWithContext: StepWithContext): Option[Try[Step]] = None
  def updateGeneralization(step: Step.Generalization, stepWithContext: StepWithContext): Option[Try[Step]] = None
  def updateNaming(step: Step.Naming, stepWithContext: StepWithContext): Option[Try[Step]] = None
  def updateSubProof(step: Step.SubProof, stepWithContext: StepWithContext): Option[Try[Step]] = None
  def updateElided(step: Step.Elided, stepWithContext: StepWithContext): Option[Try[Step]] = None
  def updateExistingStatementExtraction(step: Step.ExistingStatementExtraction, stepWithContext: StepWithContext): Option[Try[Step]] = None

  def updateStepWithSubsteps(step: Step.WithSubsteps, stepWithContext: StepWithContext): Try[Step] = {
    updateSteps(stepWithContext.forSubsteps(step))
      .map(step.replaceSubsteps(_, stepWithContext.stepProvingContext.stepContext))
  }
  def updateStepWithoutSubsteps(step: Step.WithoutSubsteps, stepWithContext: StepWithContext): Try[Step] = {
    Success(step)
  }
}
