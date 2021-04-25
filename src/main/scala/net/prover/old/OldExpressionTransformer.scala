package net.prover.old

import net.prover._
import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.expressions._
import net.prover.model.{Inference, Substitutions, VariableDefinitions}
import scalaz.{Functor, Monad}
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

  trait FunctorExpressionTransformer[TOutput[+_], TParameters] extends OldExpressionTransformer[TOutput, TParameters] {
    implicit def functor: Functor[TOutput]
  }

  trait ExpressionSequenceTransformer[TOutput[+_], TParameters]
      extends OldExpressionTransformer[TOutput, TParameters]
      with FunctorExpressionTransformer[TOutput, TParameters]
  {
    def transformGenericExpressionsWithContext[
      TExpression <: Expression](
      expressions: Seq[TExpression],
      parameters: TParameters,
      transformExpression: (TExpression, TParameters) => TOutput[TExpression])(
      implicit context: ContextWithInternalDepth
    ): TOutput[Seq[TExpression]]

    def transformExpressionsWithContext(
      expressions: Seq[Expression],
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[Seq[Expression]] = {
      transformGenericExpressionsWithContext(expressions, parameters, transformExpressionWithContext)
    }
    def transformStatementsWithContext(
      expressions: Seq[Statement],
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[Seq[Statement]] = {
      transformGenericExpressionsWithContext(expressions, parameters, transformStatementWithContext)
    }
    def transformTermsWithContext(
      expressions: Seq[Term],
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[Seq[Term]] = {
      transformGenericExpressionsWithContext(expressions, parameters, transformTermWithContext)
    }
  }

  trait TraversableExpressionTransformer[TOutput[+_], TParameters] extends ExpressionSequenceTransformer[TOutput, TParameters] with FunctorExpressionTransformer[TOutput, TParameters] {
    def traverse[T](seq: Seq[TOutput[T]]): TOutput[Seq[T]]

    override def transformGenericExpressionsWithContext[
      TExpression <: Expression](
      expressions: Seq[TExpression],
      parameters: TParameters,
      transformExpression: (TExpression, TParameters) => TOutput[TExpression])(
      implicit context: ContextWithInternalDepth
    ): TOutput[Seq[TExpression]] = {
      traverse(expressions.map(transformExpression(_, parameters)))
    }
  }

  trait MonadExpressionTransformer[TOutput[+_], TParameters] extends TraversableExpressionTransformer[TOutput, TParameters] {
    implicit def monad: Monad[TOutput]
    override implicit def functor: Functor[TOutput] = monad

    override def traverse[T](seq: Seq[TOutput[T]]): TOutput[Seq[T]] = {
      seq.foldLeft(monad.point(Seq.empty[T])) { case (outputSeq, outputValue) =>
        for {
          seq <- outputSeq
          value <- outputValue
        } yield seq :+ value
      }
    }

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

  trait IdentityExpressionTransformer[TParameters] extends MonadExpressionTransformer[Identity, TParameters] {
    override implicit val monad: Monad[Identity] = identityMonad
  }
  trait OptionExpressionTransformer[TParameters] extends MonadExpressionTransformer[Option, TParameters] {
    override implicit val monad: Monad[Option] = scalaz.std.option.optionInstance
  }
  trait TryExpressionTransformer[TParameters] extends MonadExpressionTransformer[Try, TParameters] {
    override implicit val monad: Monad[Try] = tryMonad
  }

  trait DefaultVariableTransformation[TOutput[+_], TParameters]
      extends ExpressionSequenceTransformer[TOutput, TParameters]
      with OldExpressionTransformer.WithCommonVariableTransformation[TOutput, TParameters]
  {
    override def transformExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TExpression],
      TExpression <: Expression](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TExpression] = {
      transformTermsWithContext(expressionVariable.arguments, parameters)
        .map(expressionVariable.update)
    }
  }
  trait DefaultCompoundExpressionTransformation[TOutput[+_], TParameters]
      extends ExpressionSequenceTransformer[TOutput, TParameters]
      with FunctorExpressionTransformer[TOutput, TParameters]
      with OldExpressionTransformer.WithCommonCompoundExpressionTransformation[TOutput, TParameters] {
    override def transformCompoundExpressionWithContext[
      TCompoundExpression <: DefinedExpression[TExpression],
      TExpression <: Expression](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TExpression] = {
      val innerContext = context.increaseDepth(compoundExpression)
      transformExpressionsWithContext(compoundExpression.components, parameters)(innerContext)
        .map(compoundExpression.updateComponents)
    }
  }
}
