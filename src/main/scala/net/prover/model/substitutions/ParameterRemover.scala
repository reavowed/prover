package net.prover.model.substitutions

import net.prover.core.expressions._
import net.prover.core.transformers.{ContextWithInternalDepth, ExpressionTransformer}

object ParameterRemover
    extends ExpressionTransformer.OptionExpressionTransformer[Int]
    with ExpressionTransformer.DefaultVariableTransformation[Option, Int]
    with ExpressionTransformer.DefaultCompoundExpressionTransformation[Option, Int]
{
  override def transformParameterWithContext(
    parameter: Parameter,
    numberOfParametersToRemove: Int)(
    implicit context: ContextWithInternalDepth
  ): Option[Parameter] = {
    if (parameter.level < context.internalDepth)
      Some(parameter)
    else if (parameter.level < context.internalDepth + numberOfParametersToRemove)
      None
    else
      Some(Parameter(parameter.index, parameter.level - numberOfParametersToRemove))
  }
}
