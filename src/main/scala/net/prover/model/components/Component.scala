package net.prover.model.components

import net.prover.model.{DistinctVariables, Parser, ParsingContext, PartialSubstitutions, Substitutions}

trait Component {
  def componentType: ComponentType

  /**
    * All the variables that appear syntactically in this component and must
    * be supplied in a general substitution.
    */
  def allVariables: Set[Variable]

  /**
    * The variables that definitely appear in this component and to which
    * distinct variable conditions must be carried over. Mostly the same as
    * `allVariables`, except that `x` is not present in [y/x]φ.
    */
  def presentVariables: Set[Variable]
  def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable])
  def boundVariables: Set[TermVariable] = boundAndFreeVariables._1
  def freeVariables: Set[TermVariable] = boundAndFreeVariables._2
  def implicitDistinctVariables: DistinctVariables
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable]
  def getDifferences(other: Component): Set[(Component, Component)] = {
    if (this == other)
      Set.empty
    else
      getInternalDifferences(other) + (this -> other)
  }
  def getInternalDifferences(other: Component): Set[(Component, Component)] = Set.empty
  def calculateSubstitutions(other: Component, substitutions: PartialSubstitutions): Seq[PartialSubstitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Component]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Component]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Component, DistinctVariables)]
  def validateSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables]
  def findSubstitution(target: Component, termVariableToBeReplaced: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
  def findDoubleSubstitution(
    target: Component,
    firstTermVariable: TermVariable,
    firstTerm: Term,
    secondTermVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
  def replacePlaceholder(other: Component): Option[Component]
  def condense(
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    condenseOneWay(other, thisSubstitutions, otherSubstitutions) orElse
      other.condenseOneWay(this, otherSubstitutions, thisSubstitutions).map(_.reverse)
  }
  protected def condenseOneWay(
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    for {
      thisSubstituted <- applySubstitutions(thisSubstitutions.knownSubstitutions)
      updatedOtherSubstitutions <- other.calculateSubstitutions(thisSubstituted, otherSubstitutions).headOption
    } yield (thisSubstitutions, updatedOtherSubstitutions)
  }
  def condenseWithSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    other match {
      case statementVariable: StatementVariable =>
        otherSubstitutions.unknown.toSeq.mapCollect {
          case (SubstitutedStatementVariable(`statementVariable`, otherTermToReplaceWith, otherTermToBeReplaced), otherTarget) =>
            otherSubstitutions.known.get(otherTermToBeReplaced).flatMap(Term.optionAsVariable).flatMap { substitutedTermToBeReplaced =>
              for {
                (substitutedOtherTermToReplaceWith, newDistinctVariables) <-
                  findDoubleSubstitution(otherTarget, termToBeReplaced, termToReplaceWith, substitutedTermToBeReplaced)
                    ._1.headOption
                updatedOtherSubstitutions <- otherTermToReplaceWith
                  .calculateSubstitutions(
                    substitutedOtherTermToReplaceWith,
                    otherSubstitutions.withDistinctVariables(newDistinctVariables))
                  .headOption
              } yield (thisSubstitutions.withDistinctVariables(newDistinctVariables), updatedOtherSubstitutions)
            }
          case _ =>
            None
        }.headOption
      case _ =>
        None
    }
  }
  def html: String
  def safeHtml: String = html
  def serialized: String
  override def toString: String = html
}

object Component {
  def parser(implicit parsingContext: ParsingContext): Parser[Component] = {
    Statement.parser.tryOrElse(Term.parser)
  }
}
