package net.prover.utilities

import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.expressions._
import scalaz.PlusEmpty

trait ExpressionCalculator[TOutput, TParameters] {
  def calculateFromExpressionWithoutContext(expression: Expression, parameters: TParameters): TOutput = {
    calculateFromExpressionWithContext(expression, parameters)(ContextWithInternalDepth.zero)
  }
  def calculateFromExpressionWithContext(expression: Expression, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = expression match {
    case statementVariable: StatementVariable =>
      calculateFromStatementVariableWithContext(statementVariable, parameters)
    case compoundStatement: DefinedStatement =>
      calculateFromCompoundStatementWithContext(compoundStatement, parameters)
    case termVariable: TermVariable =>
      calculateFromTermVariableWithContext(termVariable, parameters)
    case compoundTerm: DefinedTerm =>
      calculateFromCompoundTermWithContext(compoundTerm, parameters)
    case parameter: FunctionParameter =>
      calculateFromParameterWithContext(parameter, parameters)
  }
  def calculateFromStatementVariableWithContext(statementVariable: StatementVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromTermVariableWithContext(termVariable: TermVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundStatementWithContext(compoundStatement: DefinedStatement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundTermWithContext(compoundTerm: DefinedTerm, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromParameterWithContext(parameter: FunctionParameter, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
}

object ExpressionCalculator {
  trait ReducableExpressionCalculator[TOutput, TParameters] extends ExpressionCalculator[TOutput, TParameters] {
    def reduceAll(outputs: Seq[TOutput]): TOutput
    def calculateFromExpressionsWithContext(expressions: Seq[Expression], parameters: TParameters)(implicit context: ContextWithInternalDepth) : TOutput = {
      reduceAll(expressions.map(calculateFromExpressionWithContext(_, parameters)(context)))
    }
  }
  trait PlusEmptyExpressionCalculator[TOutputOuter[_], TOutputInner, TParameters] extends ReducableExpressionCalculator[TOutputOuter[TOutputInner], TParameters] {
    def plusEmpty: PlusEmpty[TOutputOuter]
    override def reduceAll(outputs: Seq[TOutputOuter[TOutputInner]]): TOutputOuter[TOutputInner] = outputs.foldLeft(plusEmpty.empty[TOutputInner])(plusEmpty.plus(_, _))
  }
  trait SetExpressionCalculator[TOutput, TParameters] extends PlusEmptyExpressionCalculator[Set, TOutput, TParameters] {
    override def plusEmpty: PlusEmpty[Set] = scalaz.std.set.setInstance
  }

  trait WithCommonVariableCalculation[TOutput, TParameters] extends ExpressionCalculator[TOutput, TParameters] {
    def calculateFromStatementVariableWithContext(
      statementVariable: StatementVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput = {
      calculateFromExpressionVariableWithContext[StatementVariable, Statement](statementVariable, parameters)
    }
    def calculateFromTermVariableWithContext(
      termVariable: TermVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput = {
      calculateFromExpressionVariableWithContext[TermVariable, Term](termVariable, parameters)
    }
    def calculateFromExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TExpression],
      TExpression <: Expression](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
  trait WithDefaultVariableCalculation[TOutput, TParameters]
      extends WithCommonVariableCalculation[TOutput, TParameters]
      with ReducableExpressionCalculator[TOutput, TParameters]
  {
    override def calculateFromExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TExpression],
      TExpression <: Expression](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput = {
      calculateFromExpressionsWithContext(expressionVariable.arguments, parameters)
    }
  }

  trait WithCommonCompoundExpressionCalculation[TOutput, TParameters] extends ExpressionCalculator[TOutput, TParameters] {
    def calculateFromCompoundStatementWithContext(
      compoundStatement: DefinedStatement,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput = {
      calculateFromCompoundExpressionWithContext[DefinedStatement, Statement](compoundStatement, parameters)
    }
    def calculateFromCompoundTermWithContext(
      compoundTerm: DefinedTerm,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput = {
      calculateFromCompoundExpressionWithContext[DefinedTerm, Term](compoundTerm, parameters)
    }
    def calculateFromCompoundExpressionWithContext[
      TCompoundExpression <: DefinedExpression[TExpression],
      TExpression <: Expression](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
  trait WithDefaultCompoundExpressionCalculation[TOutput, TParameters]
      extends WithCommonCompoundExpressionCalculation[TOutput, TParameters]
      with ReducableExpressionCalculator[TOutput, TParameters]
  {
    override def calculateFromCompoundExpressionWithContext[
      TCompoundExpression <: DefinedExpression[TExpression],
      TExpression <: Expression](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput = {
      val innerContext = context.increaseDepth(compoundExpression)
      calculateFromExpressionsWithContext(compoundExpression.components, parameters)(innerContext)
    }
  }
}




