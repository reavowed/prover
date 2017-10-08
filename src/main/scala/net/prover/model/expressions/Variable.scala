package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}

trait Variable {
  def name: String
  def expression: Expression
  def expressionParser(parameterList: Seq[String])(implicit context: ParsingContext): Parser[Expression]
  def depthDifference(expression: Expression): Option[Int]
}

object Variable {
  def parser(implicit context: ParsingContext): Parser[Variable] = {
    for {
      variableName <- Parser.singleWord
    } yield {
      context.statementVariableNames.find(_ == variableName).map(StatementVariable)
        .orElse(context.RecognisedTermVariable.unapply(variableName))
        .getOrElse(throw new Exception(s"Unrecognised variable name '$variableName'"))
    }
  }

  implicit class VariableSeqOps(variables: Seq[Variable]) {
    def expressionsParser(parameterList: Seq[String])(implicit context: ParsingContext) = {
      variables.map(_.expressionParser(parameterList)).traverseParser
    }
  }
}
