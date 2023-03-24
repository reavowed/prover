package net.prover.theorems

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Step, StepContext}

object FindStep {
  def apply(theorem: Theorem, proofIndex: Int, stepIndexes: Seq[Int]): Option[(Step, StepContext)] = {
    theorem.proofs.lift(proofIndex).flatMap(apply(_, theorem.initialStepContext, stepIndexes))
  }

  def apply(proof: Proof, initialStepContext: StepContext, stepIndexes: Seq[Int]): Option[(Step, StepContext)] = {
    stepIndexes match {
      case Nil =>
        None
      case head +: tail =>
        val initialStepContextAndPathOption = proof.steps.splitAtIndexIfValid(head).map { case (before, step, _) =>
          (step, initialStepContext.addSteps(before).atIndex(head), Seq(head))
        }
        tail.foldLeft(initialStepContextAndPathOption) { case (currentStepContextAndPathOption, index) =>
          currentStepContextAndPathOption.flatMap { case (step, stepContext, path) =>
            getSubstep(step, stepContext, index).map { case (newStep, newStepContext) => (newStep, newStepContext, path :+ index) }
          }
        }.map { case (step, stepContext, _) => (step, stepContext) }
    }
  }

  def getSubstep(step: Step, outerStepContext: StepContext, index: Int): Option[(Step, StepContext)] = {
    step match {
      case step: Step.WithSubsteps =>
        step.substeps.splitAtIndexIfValid(index).map { case (before, substep, _) =>
          val innerStepContext = step.specifyStepContext(outerStepContext).addSteps(before).atIndex(index)
          (substep, innerStepContext)
        }
      case _: Step.WithoutSubsteps =>
        None
    }
  }
}
