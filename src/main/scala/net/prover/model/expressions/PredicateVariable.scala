package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

case class PredicateVariable(name: String) extends Predicate with Variable {
  override def apply(term: Term) = PredicateApplication(this, term)

  override def boundVariables = Set.empty
  override def requiredSubstitutions = Substitutions.Required(Nil, Seq(this))
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = ???
  override def applySubstitutions(substitutions: Substitutions) = ???
  override def replacePlaceholder(other: Expression) = ???

  override def serialized = s"named $name"
  override def toString = name
  override def safeToString = name

  def expressionParser(implicit context: ParsingContext) = Predicate.parser
  def applicativeParser(implicit context: ParsingContext) = ???
}

object PredicateVariable {
  def parser(implicit context: ParsingContext): Parser[PredicateVariable] = {
    Parser.singleWord.map(PredicateVariable.apply)
  }
}