package net.prover.model.substitutions

import net.prover.core.expressions._
import net.prover.core.transformers.ContextWithInternalDepth

import scala.reflect.ClassTag

trait ExpressionCalculator[TOutput, TParameters] {
  def calculateFromExpressionWithoutContext(expression: Expression, parameters: TParameters): TOutput = calculateFromExpressionWithContext(expression, parameters)(ContextWithInternalDepth.zero)
  def calculateFromExpressionWithContext(expression: Expression, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = expression match {
    case statementVariable: StatementVariable => calculateFromStatementVariableWithContext(statementVariable, parameters)
    case compoundStatement: CompoundStatement => calculateFromCompoundStatementWithContext(compoundStatement, parameters)
    case termVariable: TermVariable => calculateFromTermVariableWithContext(termVariable, parameters)
    case compoundTerm: CompoundTerm => calculateFromCompoundTermWithContext(compoundTerm, parameters)
    case parameter: Parameter => calculateFromParameterWithContext(parameter, parameters)
  }
  def calculateFromStatementVariableWithContext(statementVariable: StatementVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromTermVariableWithContext(termVariable: TermVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundStatementWithContext(compoundStatement: CompoundStatement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundTermWithContext(compoundTerm: CompoundTerm, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromParameterWithContext(parameter: Parameter, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
}

object ExpressionCalculator {
  trait WithCommonVariableCalculation[TOutput, TParameters] extends ExpressionCalculator[TOutput, TParameters] {
    override def calculateFromStatementVariableWithContext(baseStatementVariable: StatementVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromExpressionVariableWithContext(baseStatementVariable, parameters)
    }
    override def calculateFromTermVariableWithContext(baseTermVariable: TermVariable, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromExpressionVariableWithContext(baseTermVariable, parameters)
    }
    def calculateFromExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TVariable, TExpression],
      TExpression <: Expression : ClassTag](
      expressionVariable: TVariable,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
  trait WithCommonCompoundExpressionCalculation[TOutput, TParameters] extends ExpressionCalculator[TOutput, TParameters] {
    def calculateFromCompoundStatementWithContext(compoundStatement: CompoundStatement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromCompoundExpressionWithContext(compoundStatement, parameters)
    }
    def calculateFromCompoundTermWithContext(compoundTerm: CompoundTerm, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromCompoundExpressionWithContext(compoundTerm, parameters)
    }
    def calculateFromCompoundExpressionWithContext[
      TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression],
      TExpression <: Expression : ClassTag](
      compoundExpression: TCompoundExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
}
