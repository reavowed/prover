package net.prover.model.substitutions
import net.prover.core.expressions.{CompoundExpression, Expression, ExpressionVariable, Parameter}
import net.prover.core.transformers.ContextWithInternalDepth

import scala.reflect.ClassTag

class StructuralComplexityCalculator
    extends ExpressionCalculator[Int, Unit]
    with ExpressionCalculator.WithCommonVariableCalculation[Int, Unit]
    with ExpressionCalculator.WithCommonCompoundExpressionCalculation[Int, Unit]
{
  override def calculateFromExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TVariable, TExpression],
    TExpression <: Expression : ClassTag](
    expressionVariable: TVariable,
    parameters: Unit)(
    implicit context: ContextWithInternalDepth
  ): Int = {
    0
  }
  override def calculateFromCompoundExpressionWithContext[
    TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression],
    TExpression <: Expression : ClassTag](
    compoundExpression: TCompoundExpression,
    parameters: Unit)(
    implicit context: ContextWithInternalDepth
  ): Int = {
    compoundExpression.components.map(calculateFromCompoundExpressionWithContext(_, parameters)).sum + (compoundExpression.components.length max 1)
  }
  override def calculateFromParameterWithContext(
    parameter: Parameter,
    parameters: Unit)(
    implicit context: ContextWithInternalDepth
  ): Int = {
    1
  }
}
