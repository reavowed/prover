package net.prover.theorems

import net.prover.entries.{ProofWithContext, StepWithContext, StepsWithContext, TheoremWithContext}
import net.prover.model.proof.Step

object FindStep {
  def apply(theoremWithContext: TheoremWithContext, proofIndex: Int, stepIndexes: Seq[Int]): Option[StepWithContext] = {
    theoremWithContext.proofsWithContext.lift(proofIndex).flatMap(apply(_, stepIndexes))
  }

  def apply(proofWithContext: ProofWithContext, stepIndexes: Seq[Int]): Option[StepWithContext] = {
    apply(proofWithContext.stepsWithContext, stepIndexes)
  }

  def apply(stepsWithContext: StepsWithContext, stepIndexes: Seq[Int]): Option[StepWithContext] = {
    stepIndexes match {
      case Nil =>
        None
      case head +: tail =>
        stepsWithContext.atIndex(head).flatMap(apply(_, tail))
    }
  }

  def apply(stepWithContext: StepWithContext, stepIndexes: Seq[Int]): Option[StepWithContext] = {
    if (stepIndexes.isEmpty)
      Some(stepWithContext)
    else stepWithContext.step match {
      case step: Step.WithSubsteps =>
        apply(stepWithContext.forSubsteps(step), stepIndexes)
      case _ =>
        None
    }
  }
}
