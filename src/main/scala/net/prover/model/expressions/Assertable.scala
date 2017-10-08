package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Assertable extends Expression {
  def applySubstitutions(substitutions: Substitutions): Option[Assertable]
  def replacePlaceholder(other: Expression): Assertable
  def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions): Seq[(Predicate, Substitutions)]
  def makeApplicative: Option[Predicate]
  def increaseDepth(additionalDepth: Int): Predicate
}

object Assertable {
  def parser(implicit parsingContext: ParsingContext): Parser[Assertable] = {
    if (parsingContext.parameterLists.nonEmpty)
      Predicate.parser
    else
      Statement.parser
  }
}