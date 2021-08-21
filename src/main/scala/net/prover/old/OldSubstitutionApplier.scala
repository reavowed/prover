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
    transform: (TExpression, SubstitutionSpecificationParameters) => Try[TExpression],
    description: String)(
    implicit context: ContextWithInternalDepth
  ): Try[TExpression] = {
    for {
      predicate <- substitutionLens.get(parameters.substitutions).lift(expressionVariable.index).orExceptionWithMessage(s"No substitution ${description} with index ${expressionVariable.index} found")
      result <- transform(predicate, SubstitutionSpecificationParameters(expressionVariable.arguments, parameters.substitutions, context, parameters.externalContext))
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
}

trait SubstitutionApplicationImplicits {
  implicit class ExpressionSubstitutionApplier(expression: Expression) {
    def applySubstitutions(substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Expression] = {
      OldSubstitutionApplier.transformExpressionWithoutContext(expression, ApplicationParameters(substitutions, contextWithExternalDepth))
    }
  }
  implicit class StatementSubstitutionApplier(statement: Statement) {
    def applySubstitutions(substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Statement] = {
      OldSubstitutionApplier.transformStatementWithoutContext(statement, ApplicationParameters(substitutions, contextWithExternalDepth))
    }
    def applySubstitutionsInsideStep(substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Statement] = {
      OldSubstitutionApplier.transformStatementWithContext(statement, ApplicationParameters(substitutions, contextWithExternalDepth))(ContextWithInternalDepth(1))
    }
  }
  implicit class TermSubstitutionApplier(term: Term) {
    def applySubstitutions(substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Term] = {
      OldSubstitutionApplier.transformTermWithoutContext(term, ApplicationParameters(substitutions, contextWithExternalDepth))
    }
  }
  implicit class RuleOfInferenceSubstitutionApplier(term: Inference) {
    def applySubstitutions(substitutions: Substitutions)(implicit contextWithExternalDepth: ContextWithExternalDepth): Try[Inference] = {
      OldSubstitutionApplier.transformRuleOfInference(term, ApplicationParameters(substitutions, contextWithExternalDepth))
    }
  }
}
