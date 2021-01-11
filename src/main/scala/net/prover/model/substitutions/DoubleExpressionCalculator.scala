package net.prover.model.substitutions

import net.prover.core.expressions._
import net.prover.core.transformers.ContextWithInternalDepth

import scala.reflect.ClassTag

trait DoubleExpressionCalculator[TOutput, TParameters] {
  def flattenOption(option: Option[TOutput]): TOutput

  def matchVariable[
    TVariable <: ExpressionVariable[TVariable, TExpression],
    TExpression <: Expression : ClassTag](
    baseVariable: TVariable,
    targetExpression: TExpression,
  ): Option[Seq[(Expression, Expression)]] = {
    targetExpression.asOptionalInstanceOf[TVariable]
      .filter(_.index == baseVariable.index)
      .flatMap(targetVariable => baseVariable.arguments.zipStrict(targetVariable.arguments))
  }
  def matchCompoundExpression[
    TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression] : ClassTag,
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
      flattenOption(targetExpression.asOptionalInstanceOf[StatementVariable].map(calculateFromStatementVariableWithContext(statementVariable, _, parameters)))
    case compoundStatement: CompoundStatement =>
      flattenOption(targetExpression.asOptionalInstanceOf[CompoundStatement].map(calculateFromCompoundStatementWithContext(compoundStatement, _, parameters)))
    case termVariable: TermVariable =>
      flattenOption(targetExpression.asOptionalInstanceOf[TermVariable].map(calculateFromTermVariableWithContext(termVariable, _, parameters)))
    case compoundTerm: CompoundTerm =>
      flattenOption(targetExpression.asOptionalInstanceOf[CompoundTerm].map(calculateFromCompoundTermWithContext(compoundTerm, _, parameters)))
    case parameter: Parameter =>
      flattenOption(targetExpression.asOptionalInstanceOf[Parameter].map(calculateFromParameterWithContext(parameter, _, parameters)))
  }
  def calculateFromStatementVariableWithContext(baseStatementVariable: StatementVariable, targetStatement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromTermVariableWithContext(baseTermVariable: TermVariable, targetTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundStatementWithContext(baseCompoundStatement: CompoundStatement, targetStatement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromCompoundTermWithContext(baseCompoundTerm: CompoundTerm, targetTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
  def calculateFromParameterWithContext(baseParameter: Parameter, targetTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput
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
      TVariable <: ExpressionVariable[TVariable, TExpression],
      TExpression <: Expression : ClassTag](
      baseExpressionVariable: TVariable,
      targetExpression: TExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
  trait WithCommonCompoundExpressionCalculation[TOutput, TParameters] extends DoubleExpressionCalculator[TOutput, TParameters] {
    def calculateFromCompoundStatementWithContext(baseCompoundStatement: CompoundStatement, targetCompoundStatement: Statement, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromCompoundExpressionWithContext(baseCompoundStatement, targetCompoundStatement, parameters)
    }
    def calculateFromCompoundTermWithContext(baseCompoundTerm: CompoundTerm, targetCompoundTerm: Term, parameters: TParameters)(implicit context: ContextWithInternalDepth): TOutput = {
      calculateFromCompoundExpressionWithContext(baseCompoundTerm, targetCompoundTerm, parameters)
    }
    def calculateFromCompoundExpressionWithContext[
      TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression],
      TExpression <: Expression : ClassTag](
      baseCompoundExpression: TCompoundExpression,
      targetExpression: TExpression,
      parameters: TParameters)(
      implicit context: ContextWithInternalDepth
    ): TOutput
  }
}


