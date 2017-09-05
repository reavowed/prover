package net.prover.model.components

import net.prover.model.entries.TermDefinition

case class DefinedTerm(
    subcomponents: Seq[Component],
    definition: TermDefinition)
  extends Term with DefinedComponent[Term]
{
  def format = definition.format
  def symbol = definition.symbol
  def scopedBoundVariableNames = Nil

  override def getMatch(other: Component): Option[Seq[Component]] = other match {
    case DefinedTerm(otherSubcomponents, `definition`) =>
      Some(otherSubcomponents)
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Component]): Term = {
    copy(subcomponents = newSubcomponents)
  }
}
