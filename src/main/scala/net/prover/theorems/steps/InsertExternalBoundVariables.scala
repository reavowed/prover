package net.prover.theorems.steps

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.util.FunctorTypes.Identity

object InsertExternalBoundVariables extends CompoundStepUpdater[InsertExternalBoundVariablesRequest, Identity] {

  def apply(steps: Seq[Step], stepContext: StepContext, numberOfParametersToInsert: Int): Seq[Step] = {
    apply(steps.toList, stepContext, InsertExternalBoundVariablesRequest(numberOfParametersToInsert, stepContext))
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
