package net.prover.substitutionFinding.transformers

import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.expressions.FunctionParameter
import net.prover.old.OldExpressionTransformer

object ParameterRemover
    extends OldExpressionTransformer.OptionExpressionTransformer[Int]
    with OldExpressionTransformer.DefaultVariableTransformation[Option, Int]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Option, Int]
{
  override def transformParameterWithContext(
    parameter: FunctionParameter,
    numberOfParametersToRemove: Int
  )(
    implicit context: ContextWithInternalDepth
  ): Option[FunctionParameter] = {
    if (parameter.level < context.internalDepth)
      Some(parameter)
    else if (parameter.level < context.internalDepth + numberOfParametersToRemove)
      None
    else
      Some(FunctionParameter(parameter.index, parameter.level - numberOfParametersToRemove))
  }
}
