package net.prover.core.substitutions

import net.prover.core.expressions.{DefinedExpression, DefinedStatement, DefinedTerm, Expression, Parameter, Statement, StatementVariable, Term, TermVariable}

object ParameterInserter {

  case class Context(internalDepth: Int) extends ContextWithInternalDepth[Context] {
    override def withInternalDepth(newInternalDepth: Int): Context = copy(internalDepth = newInternalDepth)
  }

  def insertExternalParameters(expression: Expression, numberOfParametersToInsert: Int)(implicit context: Context): Expression = expression match {
    case statement: Statement =>
      insertExternalParameters(statement, numberOfParametersToInsert)
    case term: Term =>
      insertExternalParameters(term, numberOfParametersToInsert)
  }
  def insertExternalParameters(statement: Statement, numberOfParametersToInsert: Int)(implicit context: Context): Statement = statement match {
    case StatementVariable(index, arguments) =>
      StatementVariable(index, arguments.map(insertExternalParameters(_, numberOfParametersToInsert)))
    case definedStatement : DefinedStatement =>
      insertExternalParametersIntoDefinedExpression(definedStatement, numberOfParametersToInsert)
  }
  def insertExternalParameters(term: Term, numberOfParametersToInsert: Int)(implicit context: Context): Term = term match {
    case TermVariable(index, arguments) =>
      TermVariable(index, arguments.map(insertExternalParameters(_, numberOfParametersToInsert)))
    case definedTerm : DefinedTerm =>
      insertExternalParametersIntoDefinedExpression(definedTerm, numberOfParametersToInsert)
    case p @ Parameter(index, level) =>
      if (level >= context.internalDepth) {
        Parameter(index, level + numberOfParametersToInsert)
      } else {
        p
      }
  }

  private def insertExternalParametersIntoDefinedExpression[T <: DefinedExpression[T]](expression: T, numberOfParametersToInsert: Int)(implicit context: Context): T = {
      expression.withNewComponents(expression.components.map(insertExternalParameters(_, numberOfParametersToInsert)(context.increaseDepth(expression))))
  }
}
