package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Objectable extends Expression {
  def applySubstitutions(substitutions: Substitutions): Option[Objectable]
  def replacePlaceholder(other: Expression): Objectable
  def makeApplicative = None
  def calculateApplicatives(
    arguments: Seq[Objectable],
    substitutions: Substitutions
  ): Seq[(Function, Substitutions)] = {
    arguments.flatMapWithIndex { case (argument, index) =>
      argument.calculateSubstitutions(this, substitutions).map(FunctionParameter.anonymous(index, 1, depth + 1) -> _)
    }
  }
  def increaseDepth(additionalDepth: Int): Function
}

object Objectable {
  def parser(implicit parsingContext: ParsingContext): Parser[Objectable] = {
    if (parsingContext.parameterLists.nonEmpty)
      Function.parser
    else
      Term.parser
  }
}