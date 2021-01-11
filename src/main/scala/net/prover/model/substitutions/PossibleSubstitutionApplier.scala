package net.prover.model.substitutions

import monocle.Lens
import monocle.macros.GenLens
import net.prover.core.expressions._
import net.prover.core.transformers.{ContextWithInternalDepth, ExpressionTransformer}
import net.prover.model

import scala.reflect.ClassTag
import scala.util.Success

case class PossibleSubstitutionApplicationParameters(substitutions: PossibleSubstitutions, externalContext: ContextWithExternalDepth)
object PossibleSubstitutionApplier
    extends ExpressionTransformer.OptionExpressionTransformer[PossibleSubstitutionApplicationParameters]
    with ExpressionTransformer.DefaultCompoundExpressionTransformation[Option, PossibleSubstitutionApplicationParameters]
{
  def transformExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TVariable, TExpression],
    TExpression <: Expression : ClassTag](
    expressionVariable: TVariable,
    parameters: PossibleSubstitutionApplicationParameters,
    substitutionLens: Lens[PossibleSubstitutions, Map[Int, TExpression]],
    transform: (TExpression, PossibleSubstitutionSpecificationParameters) => Option[TExpression],
    description: String)(
    implicit context: ContextWithInternalDepth
  ): Option[TExpression] = {
    for {
      predicate <- substitutionLens.get(parameters.substitutions).get(expressionVariable.index)
      result <- transform(predicate, PossibleSubstitutionSpecificationParameters(expressionVariable.arguments, parameters.substitutions, context, parameters.externalContext))
    } yield result
  }
  override def transformStatementVariableWithContext(
    statementVariable: StatementVariable,
    parameters: PossibleSubstitutionApplicationParameters)(
    implicit context: ContextWithInternalDepth
  ): Option[Statement] = {
    transformExpressionVariableWithContext(statementVariable, parameters, GenLens[PossibleSubstitutions](_.statements), PossibleSubstitutionSpecifier.transformStatementWithoutContext, "statement")
  }
  override def transformTermVariableWithContext(termVariable: TermVariable, parameters: PossibleSubstitutionApplicationParameters)(implicit context: ContextWithInternalDepth): Option[Term] = {
    transformExpressionVariableWithContext(termVariable, parameters, GenLens[PossibleSubstitutions](_.terms), PossibleSubstitutionSpecifier.transformTermWithoutContext, "term")
  }
  override def transformParameterWithContext(parameter: Parameter, parameters: PossibleSubstitutionApplicationParameters)(implicit context: ContextWithInternalDepth): Option[Term] = Success(parameter)

  def applySubstitutions(statement: Statement, substitutions: PossibleSubstitutions)(implicit contextWithExternalDepth: model.substitutions.ContextWithExternalDepth): Option[Statement] = {
    transformStatementWithoutContext(statement, PossibleSubstitutionApplicationParameters(substitutions, ContextWithExternalDepth(contextWithExternalDepth.externalDepth)))
  }
}
