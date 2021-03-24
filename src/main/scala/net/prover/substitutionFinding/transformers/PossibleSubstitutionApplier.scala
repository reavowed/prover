package net.prover.substitutionFinding.transformers

import monocle.Lens
import monocle.macros.GenLens
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.expressions._
import net.prover.old.OldExpressionTransformer
import net.prover.substitutionFinding.model.PossibleSubstitutions

import scala.util.{Success, Try}

case class PossibleSubstitutionApplicationParameters(substitutions: PossibleSubstitutions, externalContext: ContextWithExternalDepth)
object PossibleSubstitutionApplier
    extends OldExpressionTransformer.TryExpressionTransformer[PossibleSubstitutionApplicationParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Try, PossibleSubstitutionApplicationParameters]
{
  def transformExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TExpression],
    TExpression <: Expression](
    expressionVariable: TVariable,
    parameters: PossibleSubstitutionApplicationParameters,
    substitutionLens: Lens[PossibleSubstitutions, Map[Int, TExpression]],
    transform: (TExpression, PossibleSubstitutionSpecificationParameters) => Try[TExpression],
    description: String)(
    implicit context: ContextWithInternalDepth
  ): Try[TExpression] = {
    for {
      predicate <- substitutionLens.get(parameters.substitutions).get(expressionVariable.index).orExceptionWithMessage(s"No substitution ${description} with index ${expressionVariable.index} found")
      result <- transform(predicate, PossibleSubstitutionSpecificationParameters(expressionVariable.arguments, parameters.substitutions, context, parameters.externalContext))
    } yield result
  }
  override def transformStatementVariableWithContext(
    statementVariable: StatementVariable,
    parameters: PossibleSubstitutionApplicationParameters)(
    implicit context: ContextWithInternalDepth
  ): Try[Statement] = {
    transformExpressionVariableWithContext(statementVariable, parameters, GenLens[PossibleSubstitutions](_.statements), PossibleSubstitutionSpecifier.transformStatementWithoutContext, "statement")
  }
  override def transformTermVariableWithContext(termVariable: TermVariable, parameters: PossibleSubstitutionApplicationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = {
    transformExpressionVariableWithContext(termVariable, parameters, GenLens[PossibleSubstitutions](_.terms), PossibleSubstitutionSpecifier.transformTermWithoutContext, "term")
  }
  override def transformParameterWithContext(parameter: FunctionParameter, parameters: PossibleSubstitutionApplicationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = Success(parameter)

  def applySubstitutions(statement: Statement, substitutions: PossibleSubstitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Statement] = {
    transformStatementWithoutContext(statement, PossibleSubstitutionApplicationParameters(substitutions, contextWithExternalDepth))
  }
  def applySubstitutions(term: Term, substitutions: PossibleSubstitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Term] = {
    transformTermWithoutContext(term, PossibleSubstitutionApplicationParameters(substitutions, contextWithExternalDepth))
  }
}
