package net.prover.theorems.steps

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext}
import net.prover.model.{Inference, Substitutions}
import scalaz.Scalaz._

object RemoveExternalBoundVariables extends CompoundStepUpdater[RemoveExternalBoundVariablesRequest, Option] {
  def apply(steps: Seq[Step], stepContext: StepContext, numberOfParametersToRemove: Int): Option[Seq[Step]] = {
    apply(steps.toList, stepContext, RemoveExternalBoundVariablesRequest(numberOfParametersToRemove, stepContext))
  }

  override def updateStatement(
    statement: Statement,
    stepContext: StepContext,
    request: RemoveExternalBoundVariablesRequest
  ): Option[Statement] = {
    statement.removeExternalParameters(request.numberOfVariablesToRemove, stepContext.externalDepth - request.outerStepContext.externalDepth)
  }

  override def updateInference(
    inference: Inference.Summary,
    stepContext: StepContext,
    request: RemoveExternalBoundVariablesRequest
  ): Option[Inference.Summary] = {
    Some(inference)
  }

  override def updatePremise(
    premise: Premise,
    stepContext: StepContext,
    request: RemoveExternalBoundVariablesRequest
  ): Option[Premise] = {
    premise.removeExternalParameters(request.numberOfVariablesToRemove, stepContext.externalDepth - request.outerStepContext.externalDepth)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext,
    request: RemoveExternalBoundVariablesRequest
  ): Option[Substitutions] = {
    substitutions.removeExternalParameters(request.numberOfVariablesToRemove, stepContext.externalDepth - request.outerStepContext.externalDepth)
  }
}

case class RemoveExternalBoundVariablesRequest(numberOfVariablesToRemove: Int, outerStepContext: StepContext)
