package net.prover.model.components

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Component {
  def componentType: ComponentType

  def variables: Set[Variable]
  def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable])
  def boundVariables: Set[TermVariable] = boundAndFreeVariables._1
  def freeVariables: Set[TermVariable] = boundAndFreeVariables._2
  def getDifferences(other: Component): Set[(Component, Component)] = {
    if (this == other)
      Set.empty
    else
      getInternalDifferences(other) + (this -> other)
  }
  def getInternalDifferences(other: Component): Set[(Component, Component)] = Set.empty
  def calculateSubstitutions(other: Component, substitutions: Substitutions): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Component]
  def replacePlaceholder(other: Component): Option[Component]
  def condense(
    other: Component,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ): Option[(Substitutions, Substitutions)] = {
    condenseOneWay(other, thisSubstitutions, otherSubstitutions) orElse
      other.condenseOneWay(this, otherSubstitutions, thisSubstitutions).map(_.reverse)
  }
  protected def condenseOneWay(
    other: Component,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ): Option[(Substitutions, Substitutions)] = {
    for {
      thisSubstituted <- applySubstitutions(thisSubstitutions)
      updatedOtherSubstitutions <- other.calculateSubstitutions(thisSubstituted, otherSubstitutions).headOption
    } yield (thisSubstitutions, updatedOtherSubstitutions)
  }
  def safeToString: String = toString
  def html: String
  def safeHtml: String = html
  def serialized: String
}

object Component {
  def parser(implicit parsingContext: ParsingContext): Parser[Component] = {
    Statement.parser.tryOrElse(Term.parser)
  }
}
