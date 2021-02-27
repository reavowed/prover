package net.prover.core.substitutions

import monocle.Lens
import monocle.macros.GenLens
import net.prover.core.RuleOfInference
import net.prover.core.expressions._
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth, ExpressionTransformer}

import scala.reflect.ClassTag
import scala.util.{Success, Try}

case class ApplicationParameters(substitutions: Substitutions, externalContext: ContextWithExternalDepth)
object SubstitutionApplier
    extends ExpressionTransformer.TryExpressionTransformer[ApplicationParameters]
    with ExpressionTransformer.DefaultCompoundExpressionTransformation[Try, ApplicationParameters]
{
  def transformExpressionVariableWithContext[
    TVariable <: ExpressionVariable[TVariable, TExpression],
    TExpression <: Expression : ClassTag](
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
    transformExpressionVariableWithContext(statementVariable, parameters, GenLens[Substitutions](_.statements), SubstitutionSpecifier.transformStatementWithoutContext, "statement")
  }
  override def transformTermVariableWithContext(termVariable: TermVariable, parameters: ApplicationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = {
    transformExpressionVariableWithContext(termVariable, parameters, GenLens[Substitutions](_.terms), SubstitutionSpecifier.transformTermWithoutContext, "term")
  }
  override def transformParameterWithContext(parameter: Parameter, parameters: ApplicationParameters)(implicit context: ContextWithInternalDepth): Try[Term] = Success(parameter)

  def applySubstitutions(statement: Statement, substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Statement] = {
    transformStatementWithoutContext(statement, ApplicationParameters(substitutions, ContextWithExternalDepth(contextWithExternalDepth.externalDepth)))
  }
  def applySubstitutions(ruleOfInference: RuleOfInference, substitutions: Substitutions): Try[RuleOfInference] = transformRuleOfInference(ruleOfInference, ApplicationParameters(substitutions, ContextWithExternalDepth.zero))
}
