package net.prover.old

import monocle.Lens
import monocle.macros.GenLens
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions._

import scala.util.{Success, Try}

case class ApplicationParameters(substitutions: Substitutions, externalContext: ContextWithExternalDepth)
object OldSubstitutionApplier
    extends OldExpressionTransformer.TryExpressionTransformer[ApplicationParameters]
    with OldExpressionTransformer.DefaultCompoundExpressionTransformation[Try, ApplicationParameters]
{
  def transformExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TExpression],
    TExpression <: Expression](
    expressionVariable: TVariable,
    parameters: ApplicationParameters,
    substitutionLens: Lens[Substitutions, Seq[TExpression]],
    transform: (TExpression, SpecificationParameters) => Try[TExpression],
    description: String)(
    implicit context: ContextWithInternalDepth
  ): Try[TExpression] = {
    for {
      predicate <- substitutionLens.get(parameters.substitutions).lift(expressionVariable.index).orExceptionWithMessage(s"No substitution ${description} with index ${expressionVariable.index} found")
      result <- transform(predicate, SpecificationParameters(expressionVariable.arguments, parameters.substitutions, context, parameters.externalContext))
    } yield result
  }
  override def transformStatementVariableWithContext(
    statementVariable: StatementVariable,
    parameters: ApplicationParameters)(
    implicit context: ContextWithInternalDepth
  ): Try[Statement] = {
    transformExpressionVariableWithContext(statementVariable, parameters, GenLens[Substitutions](_.statements), OldSubstitutionSpecifier.transformStatementWithoutContext, "statement")
  }
  override def transformTermVariableWithContext(termVariable: TermVariable, parameters: ApplicationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = {
    transformExpressionVariableWithContext(termVariable, parameters, GenLens[Substitutions](_.terms), OldSubstitutionSpecifier.transformTermWithoutContext, "term")
  }
  override def transformParameterWithContext(parameter: FunctionParameter, parameters: ApplicationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = Success(parameter)

  def applySubstitutionsInsideStep(statement: Statement, substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Statement] = {
    transformStatementWithContext(statement, ApplicationParameters(substitutions, contextWithExternalDepth))(ContextWithInternalDepth(1))
  }

  def applySubstitutions(expression: Expression, substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Expression] = {
    transformExpressionWithoutContext(expression, ApplicationParameters(substitutions, contextWithExternalDepth))
  }
  def applySubstitutions(statement: Statement, substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Statement] = {
    transformStatementWithoutContext(statement, ApplicationParameters(substitutions, contextWithExternalDepth))
  }
  def applySubstitutions(term: Term, substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Term] = {
    transformTermWithoutContext(term, ApplicationParameters(substitutions, contextWithExternalDepth))
  }
  def applySubstitutions(ruleOfInference: Inference, substitutions: Substitutions): Try[Inference] = transformRuleOfInference(ruleOfInference, ApplicationParameters(substitutions, ContextWithExternalDepth.zero))
}
