package net.prover.substitutionFinding.transformers

import net.prover.model.expressions._
import net.prover.core.transformers.{ContextWithInternalDepth, ContextWithExternalDepth}

import scala.reflect.ClassTag

case class ArgumentCalculationParameters(argumentsSoFar: Map[Int, Term], substitutionCalculationContext: ContextWithInternalDepth, externalContext: ContextWithExternalDepth)
object ArgumentCalculator
    extends DoubleExpressionCalculator.OptionDoubleExpressionCalculator[Map[Int, Term], ArgumentCalculationParameters]
    with DoubleExpressionCalculator.WithCommonVariableCalculation[Option[Map[Int, Term]], ArgumentCalculationParameters]
    with DoubleExpressionCalculator.WithCommonCompoundExpressionCalculation[Option[Map[Int, Term]], ArgumentCalculationParameters]
{
  def calculateArguments(baseExpression: Expression, targetExpression: Expression, substitutionCalculationContext: ContextWithInternalDepth, externalContext: ContextWithExternalDepth): Option[Map[Int, Term]] = {
    calculateFromExpressionWithoutContext(baseExpression, targetExpression, ArgumentCalculationParameters(Map.empty, substitutionCalculationContext, externalContext))
  }

  def calculateFromExpressionsWithContext(
    baseExpressions: Seq[Expression],
    targetExpressions: Seq[Expression],
    parameters: ArgumentCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[Map[Int, Term]] = {
    baseExpressions.zipStrict(targetExpressions).flatMap(_.foldLeft(Option(parameters.argumentsSoFar)) { case (argumentsOption, (expression, target)) =>
      for {
        arguments <- argumentsOption
        result <- calculateFromExpressionWithContext(expression, target, parameters.copy(argumentsSoFar = arguments))
      } yield result
    })
  }
  override def calculateFromExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TExpression] : ClassTag,
    TExpression <: Expression : ClassTag](
    expressionVariable: TVariable,
    targetExpression: TExpression,
    parameters: ArgumentCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[Map[Int, Term]] = {
    expressionVariable.getMatch(targetExpression)
      .flatMap(arguments => calculateFromExpressionsWithContext(expressionVariable.arguments, arguments, parameters))
  }
  override def calculateFromCompoundExpressionWithContext[
    TCompoundExpression <: DefinedExpression[TExpression] : ClassTag,
    TExpression <: Expression : ClassTag](
    compoundExpression: TCompoundExpression,
    targetExpression: TExpression,
    parameters: ArgumentCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[Map[Int, Term]] = {
    val innerContext = context.increaseDepth(compoundExpression)
    compoundExpression.getMatch(targetExpression)
      .flatMap(components => calculateFromExpressionsWithContext(compoundExpression.components, components, parameters)(innerContext))
  }
  override def calculateFromParameterWithContext(
    parameter: FunctionParameter,
    targetExpression: Term,
    parameters: ArgumentCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[Map[Int, Term]] = {
    if (parameter.level == context.internalDepth + parameters.externalContext.externalDepth) {
      for {
        argumentWithoutInternalParameters <- ParameterRemover.transformTermWithoutContext(targetExpression, context.internalDepth)
        result <- parameters.argumentsSoFar.tryAdd(parameter.index, argumentWithoutInternalParameters)
      } yield result
    } else if (ParameterRemover.transformTermWithContext(targetExpression, parameters.substitutionCalculationContext.internalDepth)(context).contains(parameter)) {
      Some(parameters.argumentsSoFar)
    } else {
      None
    }
  }
}
