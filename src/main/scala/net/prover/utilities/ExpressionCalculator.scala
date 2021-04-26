package net.prover.utilities

import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.expressions._

import scala.reflect.ClassTag

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
  trait WithCommonCompoundExpressionTransformation[TOutput, TParameters] extends ExpressionCalculator[TOutput, TParameters] {
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
}




