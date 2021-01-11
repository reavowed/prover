package net.prover.model.substitutions

import net.prover.core.expressions.{CompoundExpression, CompoundExpressionType, Expression, ExpressionVariable, Parameter}
import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.definitions.{CompoundExpressionDefinition, CompoundStatementDefinition, CompoundTermDefinition}

import scala.collection.mutable
import scala.reflect.ClassTag

class DefinitionalComplexityCalculator
    extends ExpressionCalculator[Int, Unit]
    with ExpressionCalculator.WithCommonVariableCalculation[Int, Unit]
    with ExpressionCalculator.WithCommonCompoundExpressionCalculation[Int, Unit]
{
  private val complexitiesByDefinition = mutable.Map[CompoundExpressionDefinition, Int]()
  private val complexitiesByExpression = mutable.Map[Expression, Int]()

  override def calculateFromExpressionWithoutContext(expression: Expression, parameters: Unit): Int = {
    complexitiesByExpression.getOrElseUpdate(expression, super.calculateFromExpressionWithoutContext(expression, parameters))
  }

  def calculateFromCompoundExpressionType(compoundExpressionType: CompoundExpressionType): Int = {
    val definition = compoundExpressionType.asInstanceOf[CompoundExpressionDefinition]
    complexitiesByDefinition.getOrElseUpdate(definition, definition match {
      case compoundStatementDefinition: CompoundStatementDefinition =>
        compoundStatementDefinition.definingStatement.map(calculateFromExpressionWithoutContext(_, ())).getOrElse(1)
      case compoundTermDefinition: CompoundTermDefinition =>
        calculateFromExpressionWithoutContext(compoundTermDefinition.definitionPredicate, ())
    })
  }
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
    compoundExpression.components.map(calculateFromExpressionWithContext(_, parameters)).sum + calculateFromCompoundExpressionType(compoundExpression.definition)
  }
  override def calculateFromParameterWithContext(
    parameter: Parameter,
    parameters: Unit)(
    implicit context: ContextWithInternalDepth
  ): Int = {
    1
  }
}
