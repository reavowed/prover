package net.prover.theorems.steps

import net.prover.entries.{StepWithContext, StepsWithContext}
import net.prover.model.proof.Step
import scalaz.Monoid
import scalaz.Scalaz._

abstract class RecursiveStepFinder[T : Monoid] {
  def apply(stepsWithContext: StepsWithContext): T = {
    stepsWithContext.stepsWithContexts.toList.foldMap(apply)
  }

  def apply(stepWithContext: StepWithContext): T = {
    stepWithContext.step match {
      case step: Step.Target => getFromTarget(step, stepWithContext)
      case step: Step.Assertion => getFromAssertion(step, stepWithContext)
      case step: Step.Deduction => getFromDeduction(step, stepWithContext)
      case step: Step.Generalization => getFromGeneralization(step, stepWithContext)
      case step: Step.Naming => getFromNaming(step, stepWithContext)
      case step: Step.SubProof => getFromSubProof(step, stepWithContext)
      case step: Step.Elided => getFromElided(step, stepWithContext)
      case step: Step.ExistingStatementExtraction => getFromExistingStatementExtraction(step, stepWithContext)
    }
  }

  def getFromTarget(step: Step.Target, stepWithContext: StepWithContext): T = {
    Monoid[T].zero
  }

  def getFromAssertion(step: Step.Assertion, stepWithContext: StepWithContext): T = {
    Monoid[T].zero
  }

  def getFromDeduction(step: Step.Deduction, stepWithContext: StepWithContext): T = {
    apply(stepWithContext.forSubsteps(step))
  }

  def getFromGeneralization(step: Step.Generalization, stepWithContext: StepWithContext): T = {
    apply(stepWithContext.forSubsteps(step))
  }

  def getFromNaming(step: Step.Naming, stepWithContext: StepWithContext): T = {
    apply(stepWithContext.forSubsteps(step))
  }

  def getFromSubProof(step: Step.SubProof, stepWithContext: StepWithContext): T = {
    apply(stepWithContext.forSubsteps(step))
  }

  def getFromElided(step: Step.Elided, stepWithContext: StepWithContext): T = {
    apply(stepWithContext.forSubsteps(step))
  }

  def getFromExistingStatementExtraction(step: Step.ExistingStatementExtraction, stepWithContext: StepWithContext): T = {
    apply(stepWithContext.forSubsteps(step))
  }
}
