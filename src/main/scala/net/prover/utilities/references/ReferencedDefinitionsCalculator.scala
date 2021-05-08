package net.prover.utilities.references

import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.expressions.{DefinedExpression, Expression, FunctionParameter}
import net.prover.utilities.ExpressionCalculator
import net.prover.utilities.ExpressionCalculator.SetExpressionCalculator

object ReferencedDefinitionsCalculator
    extends SetExpressionCalculator[CompoundExpressionDefinition, Unit]
    with ExpressionCalculator.WithDefaultVariableCalculation[Set[CompoundExpressionDefinition], Unit]
    with ExpressionCalculator.WithDefaultCompoundExpressionTransformation[Set[CompoundExpressionDefinition], Unit]
{
  override def calculateFromCompoundExpressionWithContext[
    TCompoundExpression <: DefinedExpression[TExpression],
    TExpression <: Expression](
    compoundExpression: TCompoundExpression,
    parameters: Unit)(
    implicit context: ContextWithInternalDepth
  ): Set[CompoundExpressionDefinition] = {
    super.calculateFromCompoundExpressionWithContext[TCompoundExpression, TExpression](compoundExpression, parameters) + compoundExpression.definition
  }
  override def calculateFromParameterWithContext(
    parameter: FunctionParameter,
    parameters: Unit)(
    implicit context: ContextWithInternalDepth
  ): Set[CompoundExpressionDefinition] = {
    Set.empty
  }

}

trait ReferencedCompoundExpressionsCalculatorImplicits {
  implicit class ExpressionReferencedCompoundExpressionsCalculator(expression: Expression) {
    def referencedDefinitions: Set[CompoundExpressionDefinition] = ReferencedDefinitionsCalculator.calculateFromExpressionWithoutContext(expression, ())
  }
}
