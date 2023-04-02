package net.prover.theorems.steps

import net.prover.entries.{StepWithContext, StepsWithContext}
import net.prover.model.Substitutions
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import scalaz.Id.Id

case class InsertExternalBoundVariables(numberOfVariablesToInsert: Int, outerStepsWithContext: StepsWithContext) extends CompoundStepUpdater[Id] {
  override def updateStatement(
    statement: Statement,
    substitutionContext: SubstitutionContext
  ): Statement = {
    statement.insertExternalParameters(numberOfVariablesToInsert, substitutionContext.externalDepth - outerStepsWithContext.outerStepContext.externalDepth)
  }

  override def updatePremise(
    premise: Premise,
    stepWithContext: StepWithContext
  ): Premise = {
    premise.insertExternalParameters(numberOfVariablesToInsert, stepWithContext.stepContext.externalDepth - outerStepsWithContext.outerStepContext.externalDepth)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepWithContext: StepWithContext
  ): Substitutions = {
    substitutions.insertExternalParameters(numberOfVariablesToInsert, stepWithContext.stepContext.externalDepth - outerStepsWithContext.outerStepContext.externalDepth)
  }
}

object InsertExternalBoundVariables {
  def apply(numberOfVariablesToInsert: Int)(stepsWithContext: StepsWithContext): List[Step] = {
    InsertExternalBoundVariables(numberOfVariablesToInsert, stepsWithContext)(stepsWithContext)
  }
}
