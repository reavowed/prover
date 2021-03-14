package net.prover.old

import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.expressions.{Expression, FunctionParameter, Statement, Term}
import net.prover._

import scala.util.{Success, Try}

case class SpecificationParameters(targetArguments: Map[Int, Term], contextWithExternalDepth: ContextWithExternalDepth)
object ExpressionSpecifier
    extends OldExpressionTransformer.TryExpressionTransformer[SpecificationParameters]
    with OldExpressionTransformer.DefaultVariableTransformation[Try, SpecificationParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Try, SpecificationParameters]
{
  override def transformParameterWithContext(
    parameter: FunctionParameter,
    specificationParameters: SpecificationParameters)(
    implicit context: ContextWithInternalDepth
  ): Try[Term] = {
    if (parameter.level == context.internalDepth + specificationParameters.contextWithExternalDepth.externalDepth)
      specificationParameters.targetArguments.get(parameter.index).orExceptionWithMessage(s"No argument at index ${parameter.index}")
        .map(OldParameterInserter.insertParameters(_, context.internalDepth, 0))
    else
      Success(parameter)
  }

  def specify(expression: Expression, arguments: Seq[Term])(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Expression] = {
    transformExpressionWithoutContext(expression, SpecificationParameters(arguments.indices.zip(arguments).toMap, contextWithExternalDepth))
  }
  def specify(statement: Statement, arguments: Seq[Term])(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Statement] = {
    transformStatementWithoutContext(statement, SpecificationParameters(arguments.indices.zip(arguments).toMap, contextWithExternalDepth))
  }
}
