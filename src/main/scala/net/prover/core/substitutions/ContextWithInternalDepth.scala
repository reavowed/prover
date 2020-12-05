package net.prover.core.substitutions

import net.prover.core.expressions.CompoundExpression

trait ContextWithInternalDepth[T <: ContextWithInternalDepth[T]] {
  def internalDepth: Int
  def withInternalDepth(newInternalDepth: Int): T
    def increaseDepth(expression: CompoundExpression[_]): T = if (expression.hasBoundVariables) withInternalDepth(internalDepth + 1) else withInternalDepth(internalDepth)
}
