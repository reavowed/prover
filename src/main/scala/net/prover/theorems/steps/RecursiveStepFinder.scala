package net.prover.theorems.steps

import net.prover.model.proof.Step
import scalaz.Monoid
import scalaz.Scalaz._

abstract class RecursiveStepFinder[T : Monoid] {
  def apply(steps: Seq[Step]): T = {
    steps.toList.foldMap(apply)
  }

  def apply(step: Step): T = {
    step match {
      case step: Step.Target => getFromTarget(step)
      case step: Step.Assertion => getFromAssertion(step)
      case step: Step.Deduction => getFromDeduction(step)
      case step: Step.Generalization => getFromGeneralization(step)
      case step: Step.Naming => getFromNaming(step)
      case step: Step.SubProof => getFromSubProof(step)
      case step: Step.Elided => getFromElided(step)
      case step: Step.ExistingStatementExtraction => getFromExistingStatementExtraction(step)
    }
  }

  def getFromTarget(step: Step.Target): T
  def getFromAssertion(step: Step.Assertion): T
  def getFromDeduction(step: Step.Deduction): T
  def getFromGeneralization(step: Step.Generalization): T
  def getFromNaming(step: Step.Naming): T
  def getFromSubProof(step: Step.SubProof): T
  def getFromElided(step: Step.Elided): T
  def getFromExistingStatementExtraction(step: Step.ExistingStatementExtraction): T
}
