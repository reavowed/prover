package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}

trait ExpressionType {
  def parser(implicit context: ParsingContext): Parser[Expression]
  def applicativeParser(implicit context: ParsingContext): Parser[Applicative[Expression]]
}

object ExpressionType {
  implicit class ExpressionTypeSeqOps(expressionTypes: Seq[ExpressionType]) {
    def expressionsParser(implicit context: ParsingContext) = {
      expressionTypes.map(_.parser).traverseParser
    }
    def applicativesParser(implicit context: ParsingContext) = {
      expressionTypes.map(_.applicativeParser).traverseParser
    }
  }
}
