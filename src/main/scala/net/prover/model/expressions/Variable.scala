package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}

trait Variable extends Expression {
  def text: String
  def expressionParser(implicit context: ParsingContext): Parser[Expression]
  def applicativeParser(implicit context: ParsingContext): Parser[ExpressionFunction[Expression]]
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
    def expressionsParser(implicit context: ParsingContext) = {
      variables.map(_.expressionParser).traverseParser
    }
    def applicativesParser(implicit context: ParsingContext) = {
      variables.map(_.applicativeParser).traverseParser
    }
  }
}
