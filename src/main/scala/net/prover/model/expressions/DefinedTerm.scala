package net.prover.model.expressions

import net.prover.model.entries.TermDefinition

case class DefinedTerm(
    components: Seq[Expression],
    definition: TermDefinition)
  extends Term with DefinedExpression[Term]
{
  def format = definition.format
  def symbol = definition.symbol
  def scopedBoundVariableNames = Nil

  override def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case DefinedTerm(otherComponents, `definition`) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def update(newComponents: Seq[Expression]): Term = {
    copy(components = newComponents)
  }
}
