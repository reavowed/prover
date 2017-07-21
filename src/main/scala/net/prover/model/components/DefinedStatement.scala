package net.prover.model.components

import net.prover.model.entries.StatementDefinition

case class DefinedStatement(
    subcomponents: Seq[Component],
    localBoundVariables: Set[TermVariable],
    definition: StatementDefinition)
 extends Statement with DefinedComponent[Statement]
{
  def format = definition.format
  def symbol = definition.symbol

  override def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])] = other match {
    case DefinedStatement(otherSubcomponents, otherBoundVariables, `definition`) =>
      Some((otherSubcomponents, otherBoundVariables))
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): Statement = {
    copy(subcomponents = newSubcomponents, localBoundVariables = newBoundVariables)
  }
}
