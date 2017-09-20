package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext}

case class PredicateVariable(name: String) extends Predicate {
  override def apply(term: Term) = PredicateApplication(this, term)
  override def serialized = s"named $name"
  override def toString = name
  override def safeToString = name
}

object PredicateVariable {
  def parser(implicit context: ParsingContext): Parser[PredicateVariable] = {
    Parser.singleWord.map(PredicateVariable.apply)
  }
}