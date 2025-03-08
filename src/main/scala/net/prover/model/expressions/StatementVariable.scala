package net.prover.model.expressions

import net.prover.model._
import net.prover.parsing.KnownWordParser

case class StatementVariable(index: Int, arguments: Seq[Term]) extends ExpressionVariable[Statement] with ExpressionLenses.ForStatements with Statement {
  def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case StatementVariable(`index`, otherArguments) => Some(otherArguments)
    case _ => None
  }
  def update(newArguments: Seq[Term]): StatementVariable = StatementVariable(index, newArguments)
  def serializationPrefix: String = "s"
}

object StatementVariable {
  def apply(index: Int): StatementVariable = StatementVariable(index, Nil)

  def simpleParser(implicit context: ExpressionParsingContext): KnownWordParser[StatementVariable] = {
    context.simpleVariableParser(
      context.statementVariableDefinitionParser,
      StatementVariable(_, _),
      "statement")
  }
  def applicationParser(implicit context: ExpressionParsingContext): KnownWordParser[StatementVariable] = {
    context.variableApplicationParser(
      context.statementVariableDefinitionParser,
      StatementVariable(_, _),
      "statement")
  }
}
