package net.prover.core.transformers

import net.prover.core.expressions.{CompoundExpression, CompoundExpressionType}
import net.prover.model.expressions.DefinedExpression

case class ContextWithInternalDepth(internalDepth: Int) {
  def increaseDepth(expression: CompoundExpression[_, _]): ContextWithInternalDepth = increaseDepth(expression.definition)
  def increaseDepth(expressionType: CompoundExpressionType): ContextWithInternalDepth = copy(if (expressionType.hasBoundVariables) internalDepth + 1 else internalDepth)

  def increaseDepth(expression: DefinedExpression[_]): ContextWithInternalDepth = copy(if (expression.definition.boundVariableNames.nonEmpty) internalDepth + 1 else internalDepth)
}
object ContextWithInternalDepth {
  val zero: ContextWithInternalDepth = ContextWithInternalDepth(0)
}
