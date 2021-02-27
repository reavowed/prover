package net.prover.core.transformers

import net.prover.Identity
import net.prover.core.expressions._
import net.prover.core.transformers.ExpressionTransformer.IdentityExpressionTransformer

case class InsertionParameters(numberOfParametersToInsert: Int, externalDepthToInsertAt: Int)
object ParameterInserter extends IdentityExpressionTransformer[InsertionParameters]
    with ExpressionTransformer.DefaultVariableTransformation[Identity, InsertionParameters]
    with ExpressionTransformer.DefaultCompoundExpressionTransformation[Identity, InsertionParameters]
{
  override def transformParameterWithContext(
    parameter: Parameter,
    insertionParameters: InsertionParameters)(
    implicit context: ContextWithInternalDepth
  ): Parameter = {
    if (parameter.level >= context.internalDepth + insertionParameters.externalDepthToInsertAt) {
      Parameter(parameter.index, parameter.level + insertionParameters.numberOfParametersToInsert)
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
}
