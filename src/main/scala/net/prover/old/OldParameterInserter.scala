package net.prover.old

import net.prover.Identity
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.Substitutions
import net.prover.model.expressions.{FunctionParameter, Statement, Term}
import net.prover.model.proof.Step
import net.prover.old.OldExpressionTransformer.IdentityExpressionTransformer

case class InsertionParameters(numberOfParametersToInsert: Int, externalDepthToInsertAt: Int)
object OldParameterInserter extends IdentityExpressionTransformer[InsertionParameters]
    with OldStepTransformer[Identity, InsertionParameters]
    with OldExpressionTransformer.DefaultVariableTransformation[Identity, InsertionParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Identity, InsertionParameters]
{
  override def transformParameterWithContext(
    parameter: FunctionParameter,
    insertionParameters: InsertionParameters)(
    implicit context: ContextWithInternalDepth
  ): FunctionParameter = {
    if (parameter.level >= context.internalDepth + insertionParameters.externalDepthToInsertAt) {
      FunctionParameter(parameter.index, parameter.level + insertionParameters.numberOfParametersToInsert)
    } else {
      parameter
    }
  }

  def insertParameters(statement: Statement, numberOfParametersToInsert: Int, externalDepthToInsertAt: Int): Statement = {
    transformStatementWithoutContext(statement, InsertionParameters(numberOfParametersToInsert, externalDepthToInsertAt))
  }
  def insertParameters(term: Term, numberOfParametersToInsert: Int, externalDepthToInsertAt: Int): Term = {
    transformTermWithoutContext(term, InsertionParameters(numberOfParametersToInsert, externalDepthToInsertAt))
  }
  def insertParameters(substitutions: Substitutions, numberOfParametersToInsert: Int, externalDepthToInsertAt: Int): Substitutions = {
    Substitutions(
      substitutions.statements.map(insertParameters(_, numberOfParametersToInsert, externalDepthToInsertAt)),
      substitutions.terms.map(insertParameters(_, numberOfParametersToInsert, externalDepthToInsertAt)))
  }
  def insertParameters(step: Step, numberOfParametersToInsert: Int, externalDepthToInsertAt: Int): Step = {
    transformStepWithContext(step, contextWithExternalDepth => InsertionParameters(numberOfParametersToInsert, contextWithExternalDepth.externalDepth + externalDepthToInsertAt))(ContextWithExternalDepth.zero)
  }
}
