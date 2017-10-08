package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}

case class PredicateVariable(name: String)

object PredicateVariable {
  def parser(implicit context: ParsingContext): Parser[PredicateVariable] = {
    Parser.singleWord.map(PredicateVariable.apply)
  }
}