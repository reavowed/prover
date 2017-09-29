package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Assertable extends Expression {
  def applySubstitutions(substitutions: Substitutions): Option[Assertable]
  def replacePlaceholder(other: Expression): Assertable
  def makeApplicative(argument: Term): Option[Assertable]
}

object Assertable {
  def parser(implicit parsingContext: ParsingContext): Parser[Assertable] = Statement.parser
}