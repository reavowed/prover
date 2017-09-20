package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}

trait Predicate extends ExpressionFunction[Statement]

object Predicate {
  def parser(implicit parsingContext: ParsingContext): Parser[Predicate] = {
    Parser.selectWordParser("predicate") {
      case "constant" => Statement.parser.map(ConstantPredicate.apply)
      case "defined" => DefinedPredicate.parser
      case "named" => PredicateVariable.parser
    }
  }
}
