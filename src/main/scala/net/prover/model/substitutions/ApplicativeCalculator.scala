package net.prover.model.substitutions

import net.prover.core.expressions.{CompoundExpression, CompoundStatement, CompoundTerm, Expression, ExpressionVariable, Parameter, Statement, StatementVariable, Term, TermVariable}
import net.prover.core.transformers.{ContextWithInternalDepth, ExpressionTransformer, ParameterInserter}

import scala.reflect.ClassTag

case class ApplicativeCalculationParameters(baseArguments: Seq[Term], possibleSubstitutions: PossibleSubstitutions, substitutionCalculationContext: ContextWithInternalDepth, externalContext: ContextWithExternalDepth)
object ApplicativeCalculator
    extends ExpressionTransformer[λ[X => Iterator[(X, PossibleSubstitutions)]], ApplicativeCalculationParameters]
{
  def baseTransformTermWithContext(term: Term, parameters: ApplicativeCalculationParameters)(implicit context: ContextWithInternalDepth): Iterator[(Term, PossibleSubstitutions)] = {
    for {
      (argument, index) <- parameters.baseArguments.zipWithIndex.iterator
      argumentWithParameters = ParameterInserter.insertParameters(argument, context.internalDepth, 0)
      updatedSubstitutions <- SubstitutionCalculator.calculateFromExpressionWithContext(
        argumentWithParameters,
        term,
        SubstitutionCalculationParameters(parameters.possibleSubstitutions, parameters.externalContext))(
        ContextWithInternalDepth(context.internalDepth + parameters.substitutionCalculationContext.internalDepth))
    } yield Parameter(index, parameters.externalContext.externalDepth + context.internalDepth) -> updatedSubstitutions
  }
  def transformExpressionsWithContext(expressions: Seq[Expression], parameters: ApplicativeCalculationParameters)(implicit context: ContextWithInternalDepth): Iterator[(Seq[Expression], PossibleSubstitutions)] = {
    expressions.iterator.foldLeft(Iterator((Seq.empty[Expression], parameters.possibleSubstitutions))) { case (predicatesAndSubstitutionsSoFar, expression) =>
      for {
        (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
        (predicate, newSubstitutions) <- transformExpressionWithContext(expression, parameters.copy(possibleSubstitutions = substitutionsSoFar))
      } yield (predicatesSoFar :+ predicate, newSubstitutions)
    }
  }
  def transformExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TVariable, TExpression],
    TExpression <: Expression : ClassTag](
    expressionVariable: TVariable,
    parameters: ApplicativeCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Iterator[(TVariable, PossibleSubstitutions)] = {
    transformExpressionsWithContext(expressionVariable.arguments, parameters)
      .map(_.mapLeft(newArguments => expressionVariable.withNewArguments(newArguments.map(_.asInstanceOf[Term]))))
  }
  def transformCompoundExpressionWithContext[
    TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression],
    TExpression <: Expression : ClassTag](
    compoundExpression: TCompoundExpression,
    parameters: ApplicativeCalculationParameters)(
    implicit context: ContextWithInternalDepth
  ): Iterator[(TCompoundExpression, PossibleSubstitutions)] = {
    val innerContext = context.increaseDepth(compoundExpression)
    transformExpressionsWithContext(compoundExpression.components, parameters)(innerContext)
      .map(_.mapLeft(newComponents => compoundExpression.withNewComponents(newComponents)))
  }

  override def transformStatementVariableWithContext(statementVariable: StatementVariable, parameters: ApplicativeCalculationParameters)(implicit context: ContextWithInternalDepth): Iterator[(Statement, PossibleSubstitutions)] = {
    transformExpressionVariableWithContext(statementVariable, parameters)
  }
  override def transformTermVariableWithContext(termVariable: TermVariable, parameters: ApplicativeCalculationParameters)(implicit context: ContextWithInternalDepth): Iterator[(Term, PossibleSubstitutions)] = {
    baseTransformTermWithContext(termVariable, parameters) ++ transformExpressionVariableWithContext(termVariable, parameters)
  }
  override def transformCompoundStatementWithContext(compoundStatement: CompoundStatement, parameters: ApplicativeCalculationParameters)(implicit context: ContextWithInternalDepth): Iterator[(Statement, PossibleSubstitutions)] = {
    transformCompoundExpressionWithContext(compoundStatement, parameters)
  }
  override def transformCompoundTermWithContext(compoundTerm: CompoundTerm, parameters: ApplicativeCalculationParameters)(implicit context: ContextWithInternalDepth): Iterator[(Term, PossibleSubstitutions)] = {
    baseTransformTermWithContext(compoundTerm, parameters) ++ transformCompoundExpressionWithContext(compoundTerm, parameters)
  }
  override def transformParameterWithContext(parameter: Parameter, parameters: ApplicativeCalculationParameters)(implicit context: ContextWithInternalDepth): Iterator[(Term, PossibleSubstitutions)] = {
    (baseTransformTermWithContext(parameter, parameters).toSet ++
      (if (parameter.level >= context.internalDepth + parameters.substitutionCalculationContext.internalDepth)
        // External context
        // Shifted down to cut out the shared internal context
        Seq(Parameter(parameter.index, parameter.level - parameters.substitutionCalculationContext.internalDepth) -> parameters.possibleSubstitutions)
      else if (parameter.level < context.internalDepth)
        // Internal context after the entry point to calculateApplicatives
        Seq(parameter -> parameters.possibleSubstitutions)
      else
        // Shared internal context - must be passed in via the arguments
        Nil)
      ).iterator
  }

}
