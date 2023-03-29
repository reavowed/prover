package net.prover.theorems.steps

import net.prover.entries.StepsWithContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext}
import net.prover.model.{Inference, Substitutions}
import scalaz.Id.Id

object InsertExternalBoundVariables extends CompoundStepUpdater[InsertExternalBoundVariablesRequest, Id] {

  def apply(stepsWithContext: StepsWithContext, numberOfParametersToInsert: Int): Seq[Step] = {
    apply(stepsWithContext.steps.toList, stepsWithContext.outerStepContext, InsertExternalBoundVariablesRequest(numberOfParametersToInsert, stepsWithContext.outerStepContext))
  }

  override def updateStatement(
    statement: Statement,
    stepContext: StepContext,
    request: InsertExternalBoundVariablesRequest
  ): Statement = {
    statement.insertExternalParameters(request.numberOfVariablesToInsert, stepContext.externalDepth - request.outerStepContext.externalDepth)
  }

  override def updateInference(
    inference: Inference.Summary,
    stepContext: StepContext,
    request: InsertExternalBoundVariablesRequest
  ): Inference.Summary = {
    inference
  }

  override def updatePremise(
    premise: Premise,
    stepContext: StepContext,
    request: InsertExternalBoundVariablesRequest
  ): Premise = {
    premise.insertExternalParameters(request.numberOfVariablesToInsert, stepContext.externalDepth - request.outerStepContext.externalDepth)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext,
    request: InsertExternalBoundVariablesRequest
  ): Substitutions = {
    substitutions.insertExternalParameters(request.numberOfVariablesToInsert, stepContext.externalDepth - request.outerStepContext.externalDepth)
  }
}

case class InsertExternalBoundVariablesRequest(numberOfVariablesToInsert: Int, outerStepContext: StepContext)
