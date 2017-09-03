package net.prover.model.components

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Component {
  def componentType: ComponentType
  def requiredSubstitutions: Substitutions.Required
  def calculateSubstitutions(other: Component, substitutions: Substitutions): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Component]
  def replacePlaceholder(other: Component): Component
  def calculateApplicatives(argument: Term, substitutions: Substitutions): Seq[(Applicative[Component], Substitutions)]
  def findSubcomponent(other: Component): Option[Seq[Int]] = {
    if (this == other) {
      Some(Nil)
    } else {
      None
    }
  }
  def safeToString: String = toString
  def serialized: String
}

object Component {
  def parser(implicit parsingContext: ParsingContext): Parser[Component] = {
    Statement.parser.tryOrElse(Term.parser)
  }
}
