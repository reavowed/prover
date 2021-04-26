package net.prover.substitutionFinding.transformers

import monocle.Lens
import net.prover.model.expressions._
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.utilities.DoubleExpressionCalculator

import scala.reflect.ClassTag

case class PossibleSubstitutionCalculationParameters(possibleSubstitutionsSoFar: PossibleSubstitutions, externalContext: ContextWithExternalDepth)
object PossibleSubstitutionCalculator
    extends DoubleExpressionCalculator.OptionDoubleExpressionCalculator[PossibleSubstitutions, PossibleSubstitutionCalculationParameters]
    with DoubleExpressionCalculator.WithCommonCompoundExpressionCalculation[Option[PossibleSubstitutions], PossibleSubstitutionCalculationParameters]
{
  def calculatePossibleSubstitutions(baseExpression: Expression, targetExpression: Expression)(implicit contextWithExternalDepth: ContextWithExternalDepth): Option[PossibleSubstitutions] = {
    calculateFromExpressionWithoutContext(baseExpression, targetExpression, PossibleSubstitutionCalculationParameters(PossibleSubstitutions.empty, contextWithExternalDepth))
  }
  def calculatePossibleSubstitutions(baseExpression: Expression, targetExpression: Expression, substitutionsSoFar: PossibleSubstitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Option[PossibleSubstitutions] = {
    calculateFromExpressionWithoutContext(baseExpression, targetExpression, PossibleSubstitutionCalculationParameters(substitutionsSoFar, contextWithExternalDepth))
  }

  def calculateFromExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TExpression],
    TExpression <: Expression : ClassTag](
    baseExpressionVariable: TVariable,
    targetExpression: TExpression,
    parameters: PossibleSubstitutionCalculationParameters,
    possibleSubstitutionsLens: Lens[PossibleSubstitutions, Map[Int, TExpression]],
    possibleSubstitutionsApplicationsLens: Lens[PossibleSubstitutions, Map[Int, Seq[(Seq[Term], TExpression, Int)]]])(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
      possibleSubstitutionsLens.get(parameters.possibleSubstitutionsSoFar).get(baseExpressionVariable.index) match {
        case Some(applicative) =>
          ArgumentCalculator.calculateArguments(applicative, targetExpression, context, parameters.externalContext).flatMap { otherArguments =>
            (0 until baseExpressionVariable.arity).foldLeft(Option(parameters.possibleSubstitutionsSoFar)) { case (substitutionOptions, index) =>
              substitutionOptions.flatMap { substitutionsSoFar =>
                otherArguments.get(index).map { otherArgument =>
                  PossibleSubstitutionCalculator.calculateFromExpressionWithContext(baseExpressionVariable.arguments(index), otherArgument, PossibleSubstitutionCalculationParameters(substitutionsSoFar, parameters.externalContext))(context)
                }.getOrElse(Some(substitutionsSoFar))
              }
            }
          }
        case None =>
          if (baseExpressionVariable.arguments.isEmpty) {
            for {
              targetExpressionWithoutParameters <- ParameterRemover.removeParameters(targetExpression, context.internalDepth, 0)
              result <- parameters.possibleSubstitutionsSoFar.update(baseExpressionVariable.index, targetExpressionWithoutParameters.asInstanceOf[TExpression], possibleSubstitutionsLens)
            } yield result
          } else {
            parameters.possibleSubstitutionsSoFar
              .updateAdd(
                baseExpressionVariable.index,
                (baseExpressionVariable.arguments, targetExpression.asInstanceOf[TExpression], context.internalDepth),
                possibleSubstitutionsApplicationsLens)
              .flatMap(_.clearApplicationsWherePossible(parameters.externalContext.externalDepth))
          }
      }
  }

  override def calculateFromStatementVariableWithContext(
    baseStatementVariable: StatementVariable,
    targetStatement: Statement,
    parameters: PossibleSubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    calculateFromExpressionVariableWithContext(baseStatementVariable, targetStatement, parameters, PossibleSubstitutions.statementsLens, PossibleSubstitutions.statementApplicationsLens)
  }

  override def calculateFromTermVariableWithContext(
    baseTermVariable: TermVariable,
    targetTerm: Term,
    parameters: PossibleSubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    calculateFromExpressionVariableWithContext(baseTermVariable, targetTerm, parameters, PossibleSubstitutions.termsLens, PossibleSubstitutions.termApplicationsLens)
  }

  override def calculateFromCompoundExpressionWithContext[
    TCompoundExpression <: DefinedExpression[TExpression] : ClassTag,
    TExpression <: Expression : ClassTag](
    baseCompoundExpression: TCompoundExpression,
    targetExpression: TExpression,
    parameters: PossibleSubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    val innerContext = context.increaseDepth(baseCompoundExpression)
    matchCompoundExpression(baseCompoundExpression, targetExpression)
      .flatMap(_.foldLeft(Option(parameters.possibleSubstitutionsSoFar)) { case ((substitutionsSoFarOption, (baseArgument, targetArgument))) =>
        substitutionsSoFarOption.flatMap { substitutionsSoFar =>
          calculateFromExpressionWithContext(baseArgument, targetArgument, PossibleSubstitutionCalculationParameters(substitutionsSoFar, parameters.externalContext))(innerContext)
        }
      })
  }

  override def calculateFromParameterWithContext(
    baseParameter: FunctionParameter,
    targetExpression: Term,
    parameters: PossibleSubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    if (baseParameter == targetExpression)
      Some(parameters.possibleSubstitutionsSoFar)
    else
      None
  }

}
