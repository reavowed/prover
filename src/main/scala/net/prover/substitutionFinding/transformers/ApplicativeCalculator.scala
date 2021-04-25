package net.prover.substitutionFinding.transformers

import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.expressions.{Expression, FunctionParameter, Statement, Term}
import net.prover.old.{OldExpressionTransformer, OldParameterInserter}
import net.prover.substitutionFinding.model.PossibleSubstitutions
import scalaz.Functor

case class ApplicativeCalculatorParameters(
  targetArguments: Seq[Term],
  substitutions: PossibleSubstitutions,
  previousInternalContext: ContextWithInternalDepth,
  externalContext: ContextWithExternalDepth)

/**
  * Given an expression and a list of arguments, calculate an applicative and a set of substitutions, such that when the
  * applicative is applied to the given arguments after substitution, this expression results.
  */
object ApplicativeCalculator
    extends OldExpressionTransformer[Lambda[+[A] => Iterator[(A, PossibleSubstitutions)]], ApplicativeCalculatorParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Lambda[+[A] => Iterator[(A, PossibleSubstitutions)]], ApplicativeCalculatorParameters]
    with OldExpressionTransformer.DefaultVariableTransformation[Lambda[+[A] => Iterator[(A, PossibleSubstitutions)]], ApplicativeCalculatorParameters]
{
  def calculateApplicatives(
    expression: Expression,
    targetArguments: Seq[Term],
    substitutions: PossibleSubstitutions,
    previousInternalDepth: Int,
    contextWithExternalDepth: ContextWithExternalDepth
  ): Iterator[(Expression, PossibleSubstitutions)] = {
    transformExpressionWithoutContext(expression, ApplicativeCalculatorParameters(targetArguments, substitutions, ContextWithInternalDepth(previousInternalDepth), contextWithExternalDepth))
  }
  def calculateApplicatives(
    statement: Statement,
    targetArguments: Seq[Term],
    substitutions: PossibleSubstitutions)(
    implicit contextWithExternalDepth: ContextWithExternalDepth
  ): Iterator[(Statement, PossibleSubstitutions)] = {
    transformStatementWithoutContext(statement, ApplicativeCalculatorParameters(targetArguments, substitutions, ContextWithInternalDepth(0), contextWithExternalDepth))
  }


  override def functor: Functor[Lambda[`+A` => Iterator[(A, PossibleSubstitutions)]]] = new Functor[Lambda[`+A` => Iterator[(A, PossibleSubstitutions)]]] {
    override def map[A, B](fa: Iterator[(A, PossibleSubstitutions)])(f: A => B): Iterator[(B, PossibleSubstitutions)] = fa.map(_.mapLeft(f))
  }

  override def transformGenericExpressionsWithContext[TExpression <: Expression](
    expressions: Seq[TExpression],
    parameters: ApplicativeCalculatorParameters,
    transformExpression: (TExpression, ApplicativeCalculatorParameters) => Iterator[(TExpression, PossibleSubstitutions)])(
    implicit context: ContextWithInternalDepth
  ): Iterator[(Seq[TExpression], PossibleSubstitutions)] = {
    expressions.iterator.foldLeft(Iterator((Seq.empty[TExpression], parameters.substitutions))) { case (predicatesAndSubstitutionsSoFar, expression) =>
      for {
        (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
        (predicate, newSubstitutions) <- transformExpression(expression, parameters.copy(substitutions = substitutionsSoFar))
      } yield (predicatesSoFar :+ predicate, newSubstitutions)
    }
  }

  override def transformTermWithContext(term: Term, parameters: ApplicativeCalculatorParameters)(implicit context: ContextWithInternalDepth): Iterator[(Term, PossibleSubstitutions)] = {
    import parameters._
    (
      for {
        (argument, index) <- targetArguments.zipWithIndex.iterator
        updatedSubstitutions <- PossibleSubstitutionCalculator.calculateFromExpressionWithContext(
          OldParameterInserter.insertParameters(argument, context.internalDepth, 0),
          term,
          PossibleSubstitutionCalculationParameters(substitutions, externalContext))(
          ContextWithInternalDepth(previousInternalContext.internalDepth + context.internalDepth))
      } yield FunctionParameter(index, externalContext.externalDepth + context.internalDepth) -> updatedSubstitutions
    ) ++ super.transformTermWithContext(term, parameters)
  }

  override def transformParameterWithContext(
    parameter: FunctionParameter,
    parameters: ApplicativeCalculatorParameters)(
    implicit context: ContextWithInternalDepth
  ): Iterator[(Term, PossibleSubstitutions)] = {
    import parameters._
    if (parameter.level >= context.internalDepth + previousInternalContext.internalDepth)
      // External context
      // Shifted down to cut out the shared internal context
      Iterator(FunctionParameter(parameter.index, parameter.level - previousInternalContext.internalDepth) -> substitutions)
    else if (parameter.level < context.internalDepth)
      // Internal context after the entry point to calculateApplicatives
      Iterator(parameter -> substitutions)
    else
      // Shared internal context - must be passed in via the arguments
      Iterator.empty
  }

}
