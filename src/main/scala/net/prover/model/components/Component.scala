package net.prover.model.components

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Component {
  def componentType: ComponentType
  def boundVariables: Set[Int]
  def requiredSubstitutions: Substitutions.Required
  def calculateSubstitutions(other: Component, substitutions: Substitutions, boundVariableCount: Int): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Component]
  def replacePlaceholder(other: Component): Component
  def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int): Seq[(Applicative[Component], Substitutions)]
  def makeApplicative(argument: Term): Option[Component]
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
