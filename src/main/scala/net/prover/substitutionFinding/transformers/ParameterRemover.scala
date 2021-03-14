package net.prover.substitutionFinding.transformers

import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.Substitutions
import net.prover.model.expressions.{Expression, FunctionParameter, Statement, Term}
import net.prover.model.proof.Step
import net.prover.old.{OldExpressionTransformer, OldStepTransformer}

case class RemovalParameters(numberOfParametersToRemove: Int, externalDepthToRemoveAt: Int)
object ParameterRemover
    extends OldExpressionTransformer.OptionExpressionTransformer[RemovalParameters]
    with OldStepTransformer[Option, RemovalParameters]
    with OldExpressionTransformer.DefaultVariableTransformation[Option, RemovalParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Option, RemovalParameters]
{
  override def transformParameterWithContext(
    parameter: FunctionParameter,
    removalParameters: RemovalParameters
  )(
    implicit context: ContextWithInternalDepth
  ): Option[FunctionParameter] = {
    if (parameter.level < context.internalDepth + removalParameters.externalDepthToRemoveAt)
      Some(parameter)
    else if (parameter.level < context.internalDepth + removalParameters.externalDepthToRemoveAt + removalParameters.numberOfParametersToRemove)
      None
    else
      Some(FunctionParameter(parameter.index, parameter.level - removalParameters.numberOfParametersToRemove))
  }

  def removeParameters(expression: Expression, numberOfParametersToRemove: Int, externalDepthToRemoveAt: Int): Option[Expression] = {
    transformExpressionWithoutContext(expression, RemovalParameters(numberOfParametersToRemove, externalDepthToRemoveAt))
  }
  def removeParameters(statement: Statement, numberOfParametersToRemove: Int, externalDepthToRemoveAt: Int): Option[Statement] = {
    transformStatementWithoutContext(statement, RemovalParameters(numberOfParametersToRemove, externalDepthToRemoveAt))
  }
  def removeParameters(term: Term, numberOfParametersToRemove: Int, externalDepthToRemoveAt: Int): Option[Term] = {
    transformTermWithoutContext(term, RemovalParameters(numberOfParametersToRemove, externalDepthToRemoveAt))
  }
  def removeParameters(substitutions: Substitutions, numberOfParametersToRemove: Int, externalDepthToRemoveAt: Int): Option[Substitutions] = {
    transformSubstitutions(substitutions, RemovalParameters(numberOfParametersToRemove, externalDepthToRemoveAt))
  }
  def removeParameters(step: Step, numberOfParametersToRemove: Int, externalDepthToRemoveAt: Int): Option[Step] = {
    transformStepWithContext(step, contextWithExternalDepth => RemovalParameters(numberOfParametersToRemove, contextWithExternalDepth.externalDepth + externalDepthToRemoveAt))(ContextWithExternalDepth.zero)
  }
}
