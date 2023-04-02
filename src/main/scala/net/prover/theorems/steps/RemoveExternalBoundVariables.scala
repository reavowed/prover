package net.prover.theorems.steps

import net.prover.entries.{StepWithContext, StepsWithContext}
import net.prover.model.Substitutions
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import scalaz.Scalaz._

case class RemoveExternalBoundVariables(numberOfVariablesToRemove: Int, outerStepsWithContext: StepsWithContext) extends CompoundStepUpdater[Option] {
  override def updateStatement(
    statement: Statement,
    substitutionContext: SubstitutionContext
  ): Option[Statement] = {
    statement.removeExternalParameters(numberOfVariablesToRemove, substitutionContext.externalDepth - outerStepsWithContext.outerStepContext.externalDepth)
  }

  override def updatePremise(
    premise: Premise,
    stepWithContext: StepWithContext
  ): Option[Premise] = {
    premise.removeExternalParameters(numberOfVariablesToRemove, stepWithContext.stepContext.externalDepth - outerStepsWithContext.outerStepContext.externalDepth)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepWithContext: StepWithContext
  ): Option[Substitutions] = {
    substitutions.removeExternalParameters(numberOfVariablesToRemove, stepWithContext.stepContext.externalDepth - outerStepsWithContext.outerStepContext.externalDepth)
  }
}

object RemoveExternalBoundVariables {
  def apply(numberOfVariablesToRemove: Int)(stepsWithContext: StepsWithContext): Option[List[Step]] = {
    RemoveExternalBoundVariables(numberOfVariablesToRemove, stepsWithContext)(stepsWithContext)
  }
}
