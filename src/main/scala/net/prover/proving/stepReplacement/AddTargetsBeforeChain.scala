package net.prover.proving.stepReplacement

import net.prover.controllers.models.StepInsertionProps
import net.prover.entries.StepsWithContext
import net.prover.model.proof.Step

object AddTargetsBeforeChain {
  def apply(outerPath: Seq[Int], before: Seq[Step], newAfter: Seq[Step], newTargets: Seq[Step])(implicit stepsWithContext: StepsWithContext): (Seq[Step], StepInsertionProps) = {
    val (existingStepsBeforeTransitive, transitiveSteps) = SplitPrecedingStepsBeforeChain(before, newAfter, outerPath)
    (existingStepsBeforeTransitive ++ newTargets ++ transitiveSteps ++ newAfter, StepInsertionProps(outerPath :+ existingStepsBeforeTransitive.length, newTargets))
  }
}
