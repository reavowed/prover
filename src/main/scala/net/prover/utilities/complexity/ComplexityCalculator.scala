package net.prover.utilities.complexity

import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.definitions.{CompoundExpressionDefinition, CompoundStatementDefinition, CompoundTermDefinition}
import net.prover.model.expressions.{DefinedExpression, Expression, ExpressionVariable, FunctionParameter}
import net.prover.structure.model.entries.TypedExpressionDefinitionEntry
import net.prover.utilities.ExpressionCalculator

import scala.collection.mutable

object ComplexityCalculator
  extends ExpressionCalculator[ExpressionComplexity, mutable.Map[Expression, ExpressionComplexity]]
    with ExpressionCalculator.WithCommonVariableCalculation[ExpressionComplexity, mutable.Map[Expression, ExpressionComplexity]]
    with ExpressionCalculator.WithCommonCompoundExpressionTransformation[ExpressionComplexity, mutable.Map[Expression, ExpressionComplexity]] {

  def calculateComplexity(expression: Expression, cache: mutable.Map[Expression, ExpressionComplexity]): ExpressionComplexity = calculateFromExpressionWithoutContext(expression, cache)
  def calculateComplexity(expression: Expression): ExpressionComplexity = calculateComplexity(expression, mutable.Map.empty)
  def calculateStructuralComplexity(expression: Expression): Int = calculateComplexity(expression).structuralComplexity

  override def calculateFromExpressionWithContext(expression: Expression, parameters: mutable.Map[Expression, ExpressionComplexity])(implicit context: ContextWithInternalDepth): ExpressionComplexity = {
    parameters.getOrElseUpdate(expression, super.calculateFromExpressionWithContext(expression, parameters))
  }

  override def calculateFromExpressionVariableWithContext[TVariable <: ExpressionVariable[TExpression], TExpression <: Expression](
    expressionVariable: TVariable,
    cache: mutable.Map[Expression, ExpressionComplexity])(
    implicit context: ContextWithInternalDepth
  ): ExpressionComplexity = {
    ExpressionComplexity(0, 0)
  }

  override def calculateFromCompoundExpressionWithContext[TCompoundExpression <: DefinedExpression[TExpression], TExpression <: Expression](
    compoundExpression: TCompoundExpression,
    cache: mutable.Map[Expression, ExpressionComplexity])(
    implicit context: ContextWithInternalDepth
  ): ExpressionComplexity = {
    val componentComplexity = compoundExpression.components.map(calculateFromExpressionWithContext(_, cache))
    ExpressionComplexity(
      componentComplexity.map(_.structuralComplexity).sum + (compoundExpression.components.length max 1),
      componentComplexity.map(_.definitionComplexity).sum + calculateDefinitionComplexity(compoundExpression.definition, cache))
  }

  override def calculateFromParameterWithContext(
    parameter: FunctionParameter,
    cache: mutable.Map[Expression, ExpressionComplexity])(
    implicit context: ContextWithInternalDepth
  ): ExpressionComplexity = {
    ExpressionComplexity(1, 1)
  }

  private def calculateDefinitionComplexity(
    compoundExpressionDefinition: CompoundExpressionDefinition,
    cache: mutable.Map[Expression, ExpressionComplexity]
  ): Int = compoundExpressionDefinition match {
    case definition: CompoundStatementDefinition =>
      definition.definingStatement.map(calculateFromExpressionWithoutContext(_, cache).definitionComplexity).getOrElse(1)
    case definition: CompoundTermDefinition =>
      calculateFromExpressionWithoutContext(definition.definitionPredicate, cache).definitionComplexity
  }
}
