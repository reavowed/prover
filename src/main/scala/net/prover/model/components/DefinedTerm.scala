package net.prover.model.components

import net.prover.model.DistinctVariables
import net.prover.model.entries.TermDefinition

case class DefinedTerm(
    subcomponents: Seq[Component],
    localBoundVariables: Set[TermVariable],
    definition: TermDefinition)
  extends Term with DefinedComponent[Term]
{
  def format = definition.format
  def symbol = definition.symbol

  override def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])] = other match {
    case DefinedTerm(otherSubcomponents, otherBoundVariables, `definition`) =>
      Some((otherSubcomponents, otherBoundVariables))
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): Term = {
    copy(subcomponents = newSubcomponents, localBoundVariables = newBoundVariables)
  }

  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Term, DistinctVariables)] = {
    if (this == thisTerm && other == otherTerm) {
      Some((termVariable, DistinctVariables.empty))
    } else {
      super.resolveSingleSubstitution(other, termVariable, thisTerm, otherTerm)
    }
  }
}
