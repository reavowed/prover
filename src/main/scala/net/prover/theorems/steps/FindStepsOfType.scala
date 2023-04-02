package net.prover.theorems.steps

import net.prover.entries.{StepWithContext, StepsWithContext, TheoremWithContext, TypedStepWithContext}
import net.prover.model.proof.Step

import scala.reflect.ClassTag
import scalaz.Scalaz._

object FindStepsOfType {
  def apply[TStep <: Step : ClassTag](theoremWithContext: TheoremWithContext): List[TypedStepWithContext[TStep]] = {
    theoremWithContext.proofsWithContext.toList.flatMap(p => apply(p.stepsWithContext))
  }
  def apply[TStep <: Step : ClassTag](stepsWithContext: StepsWithContext): List[TypedStepWithContext[TStep]] = {
    stepsWithContext.stepsWithContexts.toList.foldMap(apply(_: StepWithContext))
  }

  def apply[TStep <: Step : ClassTag](stepWithContext: StepWithContext): List[TypedStepWithContext[TStep]] = {
    stepWithContext.step match {
      case step: TStep => List(stepWithContext.withStep(step))
      case step: Step.WithSubsteps => apply(stepWithContext.forSubsteps(step))
      case _ => Nil
    }
  }
}
