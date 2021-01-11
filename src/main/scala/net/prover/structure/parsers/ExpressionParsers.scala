package net.prover.structure.parsers

import net.prover.core.expressions.Expression
import net.prover.model.{ExpressionParsingContext, Parser}

object ExpressionParsers {
  def expressionParser(implicit parsingContext: ExpressionParsingContext): Parser[Expression] = {
    StatementParsers.statementParser.tryOrElse(TermParsers.termParser)
  }
}
