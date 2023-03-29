package net.prover.theorems.steps

import net.prover.entries.StepsWithContext
import net.prover.model.Substitutions
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext}
import scalaz.Scalaz._

case class RemoveExternalBoundVariables(numberOfVariablesToRemove: Int, outerStepContext: StepContext) extends CompoundStepUpdater[Option] {
  override def updateStatement(
    statement: Statement,
    stepContext: StepContext
  ): Option[Statement] = {
    statement.removeExternalParameters(numberOfVariablesToRemove, stepContext.externalDepth - outerStepContext.externalDepth)
  }

  override def updatePremise(
    premise: Premise,
    stepProvingContext: StepProvingContext
  ): Option[Premise] = {
    premise.removeExternalParameters(numberOfVariablesToRemove, stepProvingContext.stepContext.externalDepth - outerStepContext.externalDepth)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext
  ): Option[Substitutions] = {
    substitutions.removeExternalParameters(numberOfVariablesToRemove, stepContext.externalDepth - outerStepContext.externalDepth)
  }
}

object RemoveExternalBoundVariables {
  def apply(numberOfVariablesToRemove: Int, stepsWithContext: StepsWithContext): Option[List[Step]] = {
    RemoveExternalBoundVariables(numberOfVariablesToRemove, stepsWithContext.outerStepContext)(stepsWithContext)
  }
}
