package net.prover.core.substitutions

import net.prover.core.expressions.DefinedExpression

trait ContextWithInternalDepth[T <: ContextWithInternalDepth[T]] {
  def internalDepth: Int
  def withInternalDepth(newInternalDepth: Int): T
    def increaseDepth(expression: DefinedExpression[_]): T = if (expression.hasBinding) withInternalDepth(internalDepth + 1) else withInternalDepth(internalDepth)
}
