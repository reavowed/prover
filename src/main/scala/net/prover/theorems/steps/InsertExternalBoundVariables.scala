package net.prover.theorems.steps

import net.prover.entries.StepsWithContext
import net.prover.model.Substitutions
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext}
import scalaz.Id.Id

case class InsertExternalBoundVariables(numberOfVariablesToInsert: Int, outerStepContext: StepContext) extends CompoundStepUpdater[Id] {
  override def updateStatement(
    statement: Statement,
    stepContext: StepContext
  ): Statement = {
    statement.insertExternalParameters(numberOfVariablesToInsert, stepContext.externalDepth - outerStepContext.externalDepth)
  }

  override def updatePremise(
    premise: Premise,
    stepProvingContext: StepProvingContext
  ): Premise = {
    premise.insertExternalParameters(numberOfVariablesToInsert, stepProvingContext.stepContext.externalDepth - outerStepContext.externalDepth)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext
  ): Substitutions = {
    substitutions.insertExternalParameters(numberOfVariablesToInsert, stepContext.externalDepth - outerStepContext.externalDepth)
  }
}

object InsertExternalBoundVariables {
  def apply(numberOfVariablesToInsert: Int, stepsWithContext: StepsWithContext): List[Step] = {
    InsertExternalBoundVariables(numberOfVariablesToInsert, stepsWithContext.outerStepContext)(stepsWithContext)
  }
}
