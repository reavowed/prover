package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}

trait Function extends ExpressionFunction[Term]

object Function {
  def parser(implicit parsingContext: ParsingContext): Parser[Function] = {
    Parser.selectWordParser("function") {
      case "constant" => Term.parser.map(ConstantFunction.apply)
      case "identity" => Parser.constant(IdentityFunction)
    }
  }
}


