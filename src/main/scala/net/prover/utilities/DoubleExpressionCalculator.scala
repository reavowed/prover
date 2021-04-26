package net.prover.utilities

import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.expressions._

import scala.reflect.ClassTag

trait DoubleExpressionCalculator[TOutput, TParameters] {
  def flattenOption(option: Option[TOutput]): TOutput

  def matchVariable[
    TVariable <: ExpressionVariable[TExpression] : ClassTag,
    TExpression <: Expression : ClassTag](
    baseVariable: TVariable,
    targetExpression: TExpression,
  ): Option[Seq[(Expression, Expression)]] = {
    targetExpression.asOptionalInstanceOf[TVariable]
      .filter(_.index == baseVariable.index)
      .flatMap(targetVariable => baseVariable.arguments.zipStrict(targetVariable.arguments))
  }
  def matchCompoundExpression[
    TCompoundExpression <: DefinedExpression[TExpression] : ClassTag,
    TExpression <: Expression : ClassTag](
    baseCompoundExpression: TCompoundExpression,
    targetExpression: TExpression
  ): Option[Seq[(Expression, Expression)]] = {
    targetExpression.asOptionalInstanceOf[TCompoundExpression]
      .filter(_.definition == baseCompoundExpression.definition)
      .flatMap(targetCompoundExpression => baseCompoundExpression.components.zipStrict(targetCompoundExpression.components))
  }

  def calculateFromExpressionWithoutContext(baseExpression: Expression, targetExpression: Expression, parameters: TParameters): TOutput = {
    calculateFromExpressionWithContext(baseExpression, targetExpression, parameters)(ContextWithInternalDepth.zero)
  }
  def calculateFromExpressionWithContext(baseExpression: Expression, targetExpression: Expression, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = baseExpression match {
    case statementVariable: StatementVariable =>
      flattenOption(targetExpression.asOptionalInstanceOf[Statement].map(calculateFromStatementVariableWithContext(statementVariable, _, parameters)))
    case compoundStatement: DefinedStatement =>
      flattenOption(targetExpression.asOptionalInstanceOf[Statement].map(calculateFromCompoundStatementWithContext(compoundStatement, _, parameters)))
    case termVariable: TermVariable =>
      flattenOption(targetExpression.asOptionalInstanceOf[Term].map(calculateFromTermVariableWithContext(termVariable, _, parameters)))
    case compoundTerm: DefinedTerm =>
      flattenOption(targetExpression.asOptionalInstanceOf[Term].map(calculateFromCompoundTermWithContext(compoundTerm, _, parameters)))
    case parameter: FunctionParameter =>
      flattenOption(targetExpression.asOptionalInstanceOf[Term].map(calculateFromParameterWithContext(parameter, _, parameters)))
  }
  def calculateFromStatementVariableWithContext(baseStatementVariable: StatementVariable, targetStatement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromTermVariableWithContext(baseTermVariable: TermVariable, targetTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundStatementWithContext(baseCompoundStatement: DefinedStatement, targetStatement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundTermWithContext(baseCompoundTerm: DefinedTerm, targetTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromParameterWithContext(baseParameter: FunctionParameter, targetTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
}

object DoubleExpressionCalculator {
  trait OptionDoubleExpressionCalculator[TOutput, TParameters] extends DoubleExpressionCalculator[Option[TOutput], TParameters] {
    override def flattenOption(option: Option[Option[TOutput]]): Option[TOutput] = option.flatten
  }
  trait WithCommonVariableCalculation[TOutput, TParameters] extends DoubleExpressionCalculator[TOutput, TParameters] {
    override def calculateFromStatementVariableWithContext(baseStatementVariable: StatementVariable, targetStatement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromExpressionVariableWithContext(baseStatementVariable, targetStatement, parameters)
    }
    override def calculateFromTermVariableWithContext(baseTermVariable: TermVariable, targetTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromExpressionVariableWithContext(baseTermVariable, targetTerm, parameters)
    }
    def calculateFromExpressionVariableWithContext[
      TVariable <: ExpressionVariable[TExpression] : ClassTag,
      TExpression <: Expression : ClassTag](
      baseExpressionVariable: TVariable,
      targetExpression: TExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
  trait WithCommonCompoundExpressionCalculation[TOutput, TParameters] extends DoubleExpressionCalculator[TOutput, TParameters] {
    def calculateFromCompoundStatementWithContext(baseCompoundStatement: DefinedStatement, targetCompoundStatement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromCompoundExpressionWithContext(baseCompoundStatement, targetCompoundStatement, parameters)
    }
    def calculateFromCompoundTermWithContext(baseCompoundTerm: DefinedTerm, targetCompoundTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromCompoundExpressionWithContext(baseCompoundTerm, targetCompoundTerm, parameters)
    }
    def calculateFromCompoundExpressionWithContext[
      TCompoundExpression <: DefinedExpression[TExpression] : ClassTag,
      TExpression <: Expression : ClassTag](
      baseCompoundExpression: TCompoundExpression,
      targetExpression: TExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
}


