package net.prover.old

import net.prover._
import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.expressions._
import net.prover.model.proof.{Premise, Step}
import net.prover.model.{Inference, Substitutions, VariableDefinitions}
import scalaz.Monad
import scalaz.syntax.monad._

import scala.util.Try

case class TransformedInference(innerInference: Inference, premises: Seq[Statement], conclusion: Statement) extends Inference.WithCalculatedId {
  override def name: String = innerInference.name
  override def variableDefinitions: VariableDefinitions = innerInference.variableDefinitions
}

trait OldExpressionTransformer[TOutput[+_], TParameters] {
  def transformExpressionWithoutContext(expression: Expression, parameters: TParameters): TOutput[Expression] = transformExpressionWithContext(expression, parameters)(ContextWithInternalDepth.zero)
  def transformStatementWithoutContext(statement: Statement, parameters: TParameters): TOutput[Statement] = transformStatementWithContext(statement, parameters)(ContextWithInternalDepth.zero)
  def transformTermWithoutContext(term: Term, parameters: TParameters): TOutput[Term] = transformTermWithContext(term, parameters)(ContextWithInternalDepth.zero)

  def transformExpressionWithContext(expression: Expression, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Expression] = expression match {
    case statement: Statement => transformStatementWithContext(statement, parameters)
    case term: Term => transformTermWithContext(term, parameters)
  }
  def transformStatementWithContext(statement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Statement] = statement match {
    case statementVariable: StatementVariable => transformStatementVariableWithContext(statementVariable, parameters)
    case compoundStatement: DefinedStatement => transformCompoundStatementWithContext(compoundStatement, parameters)
  }
  def transformTermWithContext(term: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term] = term match {
    case termVariable: TermVariable => transformTermVariableWithContext(termVariable, parameters)
    case compoundTerm: DefinedTerm => transformCompoundTermWithContext(compoundTerm, parameters)
    case parameter: FunctionParameter => transformParameterWithContext(parameter, parameters)
  }
  def transformStatementVariableWithContext(statementVariable: StatementVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Statement]
  def transformTermVariableWithContext(termVariable: TermVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term]
  def transformCompoundStatementWithContext(compoundStatement: DefinedStatement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Statement]
  def transformCompoundTermWithContext(compoundTerm: DefinedTerm, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term]
  def transformParameterWithContext(parameter: FunctionParameter, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term]
}

object OldExpressionTransformer {
  trait WithCommonVariableTransformation[TOutput[+_], TParameters] extends OldExpressionTransformer[TOutput, TParameters] {
    def transformStatementVariableWithContext(
      statementVariable: StatementVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[Statement] = {
      transformExpressionVariableWithContext[StatementVariable, Statement](statementVariable, parameters)
    }
    def transformTermVariableWithContext(
      termVariable: TermVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[Term] = {
      transformExpressionVariableWithContext[TermVariable, Term](termVariable, parameters)
    }
    def transformExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TExpression],
      TExpression <: Expression](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TExpression]
  }
  trait WithCommonCompoundExpressionTransformation[TOutput[+_], TParameters] extends OldExpressionTransformer[TOutput, TParameters] {
    def transformCompoundStatementWithContext(
      compoundStatement: DefinedStatement,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[Statement] = {
      transformCompoundExpressionWithContext[DefinedStatement, Statement](compoundStatement, parameters)
    }
    def transformCompoundTermWithContext(
      compoundTerm: DefinedTerm,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[Term] = {
      transformCompoundExpressionWithContext[DefinedTerm, Term](compoundTerm, parameters)
    }
    def transformCompoundExpressionWithContext[
      TCompoundExpression <: DefinedExpression[TExpression],
      TExpression <: Expression](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TExpression]
  }

  trait TraversableExpressionTransformer[TOutput[+_], TParameters] extends OldExpressionTransformer[TOutput, TParameters] {
    implicit def monad: Monad[TOutput]
    def traverse[T](seq: Seq[TOutput[T]]): TOutput[Seq[T]]

    def transformRuleOfInference(ruleOfInference: Inference, parameters: TParameters): TOutput[Inference] = {
      for {
        premises <- traverse(ruleOfInference.premises.map(transformStatementWithoutContext(_, parameters)))
        conclusion <- transformStatementWithoutContext(ruleOfInference.conclusion, parameters)
      } yield TransformedInference(ruleOfInference, premises, conclusion)
    }
    def transformSubstitutions(substitutions: Substitutions, parameters: TParameters): TOutput[Substitutions] = {
      for {
        statements <- traverse(substitutions.statements.map(transformStatementWithoutContext(_, parameters)))
        terms <- traverse(substitutions.terms.map(transformTermWithoutContext(_, parameters)))
      } yield Substitutions(statements, terms)
    }
  }

  trait IdentityExpressionTransformer[TParameters] extends TraversableExpressionTransformer[Identity, TParameters] {
    override implicit val monad: Monad[Identity] = identityMonad
    override def traverse[T](seq: Seq[T]): Seq[T] = seq
  }
  trait OptionExpressionTransformer[TParameters] extends TraversableExpressionTransformer[Option, TParameters] {
    override implicit val monad: Monad[Option] = scalaz.std.option.optionInstance
    override def traverse[T](seq: Seq[Option[T]]): Option[Seq[T]] = seq.traverseOption
  }
  trait TryExpressionTransformer[TParameters] extends TraversableExpressionTransformer[Try, TParameters] {
    override implicit val monad: Monad[Try] = tryMonad
    override def traverse[T](seq: Seq[Try[T]]): Try[Seq[T]] = seq.traverseTry
  }

  trait DefaultVariableTransformation[TOutput[+_], TParameters] extends TraversableExpressionTransformer[TOutput, TParameters] with OldExpressionTransformer.WithCommonVariableTransformation[TOutput, TParameters] {
    override def transformExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TExpression],
      TExpression <: Expression](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TExpression] = {
      traverse(expressionVariable.arguments.map(transformTermWithContext(_, parameters)))
        .map(expressionVariable.update)
    }
  }
  trait DefaultCompoundExpressionTransformation[TOutput[+_], TParameters] extends TraversableExpressionTransformer[TOutput, TParameters] with OldExpressionTransformer.WithCommonCompoundExpressionTransformation[TOutput, TParameters] {
    override def transformCompoundExpressionWithContext[
      TCompoundExpression <: DefinedExpression[TExpression],
      TExpression <: Expression](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TExpression] = {
      val innerContext = context.increaseDepth(compoundExpression)
      traverse(compoundExpression.components.map(transformExpressionWithContext(_, parameters)(innerContext)))
        .map(compoundExpression.updateComponents)
    }
  }
}
