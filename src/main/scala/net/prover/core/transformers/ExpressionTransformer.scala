package net.prover.core.transformers

import net.prover._
import net.prover.core.RuleOfInference
import net.prover.core.expressions._
import scalaz.Monad
import scalaz.syntax.monad._

import scala.reflect.ClassTag
import scala.util.Try

trait ExpressionTransformer[TOutput[+_], TParameters] {
  def transformExpressionWithoutContext(expression: Expression, parameters: TParameters): TOutput[Expression] = transformExpressionWithContext(expression, parameters)(ContextWithInternalDepth.zero)
  def transformStatementWithoutContext(statement: Statement, parameters: TParameters): TOutput[Statement] = transformStatementWithContext(statement, parameters)(ContextWithInternalDepth.zero)
  def transformTermWithoutContext(term: Term, parameters: TParameters): TOutput[Term] = transformTermWithContext(term, parameters)(ContextWithInternalDepth.zero)

  def transformExpressionWithContext(expression: Expression, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Expression] = expression match {
    case statement: Statement => transformStatementWithContext(statement, parameters)
    case term: Term => transformTermWithContext(term, parameters)
  }
  def transformStatementWithContext(statement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Statement] = statement match {
    case statementVariable: StatementVariable => transformStatementVariableWithContext(statementVariable, parameters)
    case compoundStatement: CompoundStatement => transformCompoundStatementWithContext(compoundStatement, parameters)
  }
  def transformTermWithContext(term: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term] = term match {
    case termVariable: TermVariable => transformTermVariableWithContext(termVariable, parameters)
    case compoundTerm: CompoundTerm => transformCompoundTermWithContext(compoundTerm, parameters)
    case parameter: Parameter => transformParameterWithContext(parameter, parameters)
  }
  def transformStatementVariableWithContext(statementVariable: StatementVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Statement]
  def transformTermVariableWithContext(termVariable: TermVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term]
  def transformCompoundStatementWithContext(compoundStatement: CompoundStatement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Statement]
  def transformCompoundTermWithContext(compoundTerm: CompoundTerm, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term]
  def transformParameterWithContext(parameter: Parameter, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term]
}

object ExpressionTransformer {
  trait WithCommonVariableTransformation[TOutput[+_], TParameters] extends ExpressionTransformer[TOutput, TParameters] {
    def transformStatementVariableWithContext(statementVariable: StatementVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[StatementVariable] = transformExpressionVariableWithContext(statementVariable, parameters)
    def transformTermVariableWithContext(termVariable: TermVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[TermVariable] = transformExpressionVariableWithContext(termVariable, parameters)
    def transformExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TVariable, TExpression],
      TExpression <: Expression : ClassTag](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TVariable]
  }
  trait WithCommonCompoundExpressionTransformation[TOutput[+_], TParameters] extends ExpressionTransformer[TOutput, TParameters] {
    def transformCompoundStatementWithContext(compoundStatement: CompoundStatement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Statement] = transformCompoundExpressionWithContext(compoundStatement, parameters)
    def transformCompoundTermWithContext(compoundTerm: CompoundTerm, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput[Term] = transformCompoundExpressionWithContext(compoundTerm, parameters)
    def transformCompoundExpressionWithContext[
      TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression],
      TExpression <: Expression : ClassTag](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TCompoundExpression]
  }

  trait TraversableExpressionTransformer[TOutput[+_], TParameters] extends ExpressionTransformer[TOutput, TParameters] {
    implicit def monad: Monad[TOutput]
    def traverse[T](seq: Seq[TOutput[T]]): TOutput[Seq[T]]

    def transformRuleOfInference(ruleOfInference: RuleOfInference, parameters: TParameters): TOutput[RuleOfInference] = {
      for {
        premises <- traverse(ruleOfInference.premises.map(transformStatementWithoutContext(_, parameters)))
        conclusion <- transformStatementWithoutContext(ruleOfInference.conclusion, parameters)
      } yield RuleOfInference.Raw(premises, conclusion)
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

  trait DefaultVariableTransformation[TOutput[+_], TParameters] extends TraversableExpressionTransformer[TOutput, TParameters] with ExpressionTransformer.WithCommonVariableTransformation[TOutput, TParameters] {
    override def transformExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TVariable, TExpression],
      TExpression <: Expression : ClassTag](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TVariable] = {
      traverse(expressionVariable.arguments.map(transformTermWithContext(_, parameters)))
        .map(expressionVariable.withNewArguments)
    }
  }
  trait DefaultCompoundExpressionTransformation[TOutput[+_], TParameters] extends TraversableExpressionTransformer[TOutput, TParameters] with ExpressionTransformer.WithCommonCompoundExpressionTransformation[TOutput, TParameters] {
    override def transformCompoundExpressionWithContext[
      TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression],
      TExpression <: Expression : ClassTag](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput[TCompoundExpression] = {
      val innerContext = context.increaseDepth(compoundExpression)
      traverse(compoundExpression.components.map(transformExpressionWithContext(_, parameters)(innerContext)))
        .map(compoundExpression.withNewComponents)
    }
  }
}
