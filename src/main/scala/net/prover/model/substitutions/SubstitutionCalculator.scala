package net.prover.model.substitutions

import monocle.Lens
import net.prover.core.expressions._
import net.prover.core.transformers.ContextWithInternalDepth

import scala.reflect.ClassTag

case class SubstitutionCalculationParameters(possibleSubstitutionsSoFar: PossibleSubstitutions, externalContext: ContextWithExternalDepth)
object SubstitutionCalculator
    extends DoubleExpressionCalculator[Option[PossibleSubstitutions], SubstitutionCalculationParameters]
    with DoubleExpressionCalculator.WithCommonCompoundExpressionCalculation[Option[PossibleSubstitutions], SubstitutionCalculationParameters]
{
  def calculateSubstitutions(baseExpression: Expression, targetExpression: Expression)(implicit contextWithExternalDepth: ContextWithExternalDepth): Option[PossibleSubstitutions] = {
    calculateFromExpressionWithoutContext(baseExpression, targetExpression, SubstitutionCalculationParameters(PossibleSubstitutions.empty, contextWithExternalDepth))
  }
  def calculateSubstitutions(baseExpression: Expression, targetExpression: Expression, substitutionsSoFar: PossibleSubstitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Option[PossibleSubstitutions] = {
    calculateFromExpressionWithoutContext(baseExpression, targetExpression, SubstitutionCalculationParameters(substitutionsSoFar, contextWithExternalDepth))
  }

  override def flattenOption(option: Option[Option[PossibleSubstitutions]]): Option[PossibleSubstitutions] = option.flatten

  def calculateFromExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TVariable, TExpression],
    TExpression <: Expression : ClassTag](
    baseExpressionVariable: TVariable,
    targetExpression: TExpression,
    parameters: SubstitutionCalculationParameters,
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
                  SubstitutionCalculator.calculateFromExpressionWithContext(baseExpressionVariable.arguments(index), otherArgument, SubstitutionCalculationParameters(substitutionsSoFar, parameters.externalContext))(context)
                }.getOrElse(Some(substitutionsSoFar))
              }
            }
          }
        case None =>
          if (baseExpressionVariable.arguments.isEmpty) {
            for {
              targetExpressionWithoutParameters <- ParameterRemover.transformExpressionWithoutContext(targetExpression, context.internalDepth)
              result <- parameters.possibleSubstitutionsSoFar.update(baseExpressionVariable.index, targetExpressionWithoutParameters.asInstanceOf[TExpression], possibleSubstitutionsLens)
            } yield result
          } else {
            parameters.possibleSubstitutionsSoFar
              .updateAdd(
                baseExpressionVariable.index,
                (baseExpressionVariable.arguments, targetExpression.asInstanceOf[TExpression], context.internalDepth),
                possibleSubstitutionsApplicationsLens)
              .flatMap(_.clearApplicationsWherePossible(parameters.externalContext))
          }
      }
  }

  override def calculateFromStatementVariableWithContext(
    baseStatementVariable: StatementVariable,
    targetStatement: Statement,
    parameters: SubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    calculateFromExpressionVariableWithContext(baseStatementVariable, targetStatement, parameters, PossibleSubstitutions.statementsLens, PossibleSubstitutions.statementApplicationsLens)
  }

  override def calculateFromTermVariableWithContext(
    baseTermVariable: TermVariable,
    targetTerm: Term,
    parameters: SubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    calculateFromExpressionVariableWithContext(baseTermVariable, targetTerm, parameters, PossibleSubstitutions.termsLens, PossibleSubstitutions.termApplicationsLens)
  }

  override def calculateFromCompoundExpressionWithContext[
    TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression],
    TExpression <: Expression : ClassTag](
    baseCompoundExpression: TCompoundExpression,
    targetExpression: TExpression,
    parameters: SubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    val innerContext = context.increaseDepth(baseCompoundExpression)
    matchCompoundExpression(baseCompoundExpression, targetExpression)
      .flatMap(_.foldLeft(Option(parameters.possibleSubstitutionsSoFar)) { case ((substitutionsSoFarOption, (baseArgument, targetArgument))) =>
        substitutionsSoFarOption.flatMap { substitutionsSoFar =>
          calculateFromExpressionWithContext(baseArgument, targetArgument, SubstitutionCalculationParameters(substitutionsSoFar, parameters.externalContext))(innerContext)
        }
      })
  }

  override def calculateFromParameterWithContext(
    baseParameter: Parameter,
    targetExpression: Term,
    parameters: SubstitutionCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[PossibleSubstitutions] = {
    if (baseParameter == targetExpression)
      Some(parameters.possibleSubstitutionsSoFar)
    else
      None
  }

}
