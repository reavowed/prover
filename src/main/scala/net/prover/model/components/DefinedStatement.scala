package net.prover.model.components

import net.prover.model.entries.StatementDefinition

case class DefinedStatement(
    subcomponents: Seq[Component],
    definition: StatementDefinition)
 extends Statement with DefinedComponent[Statement]
{
  def format = definition.format
  def symbol = definition.symbol

  override def getMatch(other: Component): Option[(Seq[Component])] = other match {
    case DefinedStatement(otherSubcomponents, `definition`) =>
      Some(otherSubcomponents)
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Component]): Statement = {
    copy(subcomponents = newSubcomponents)
  }
}
