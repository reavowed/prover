package net.prover.utilities.references

import net.prover.core.transformers.ContextWithInternalDepth
import net.prover.model.UsedVariables
import net.prover.model.expressions.{Expression, FunctionParameter, StatementVariable, TermVariable}
import net.prover.utilities.ExpressionCalculator

object UsedVariablesCalculator
  extends ExpressionCalculator[UsedVariables, Unit]
  with ExpressionCalculator.ReducableExpressionCalculator[UsedVariables, Unit]
  with ExpressionCalculator.WithDefaultCompoundExpressionCalculation[UsedVariables, Unit]
  with ExpressionCalculator.WithDefaultVariableCalculation[UsedVariables, Unit]
{
  override def reduceAll(outputs: Seq[UsedVariables]): UsedVariables = outputs.foldTogether

  override def calculateFromParameterWithContext(parameter: FunctionParameter, parameters: Unit)(implicit context: ContextWithInternalDepth): UsedVariables = UsedVariables.empty
  override def calculateFromStatementVariableWithContext(statementVariable: StatementVariable, parameters: Unit)(implicit context: ContextWithInternalDepth): UsedVariables = {
    super.calculateFromStatementVariableWithContext(statementVariable, parameters).withStatement(statementVariable)
  }
  override def calculateFromTermVariableWithContext(termVariable: TermVariable, parameters: Unit)(implicit context: ContextWithInternalDepth): UsedVariables = {
    super.calculateFromTermVariableWithContext(termVariable, parameters).withTerm(termVariable)
  }
}

trait UsedVariablesCalculatorImplicits {
  implicit class ExpressionUsedVariablesCalculator(expression: Expression) {
    def usedVariables: UsedVariables = UsedVariablesCalculator.calculateFromExpressionWithoutContext(expression, ())
  }
  implicit class ExpressionSeqUsedVariablesCalculator(expressions: Seq[Expression]) {
    def usedVariables: UsedVariables = UsedVariablesCalculator.calculateFromExpressionsWithContext(expressions, ())(ContextWithInternalDepth.zero)
  }
}
