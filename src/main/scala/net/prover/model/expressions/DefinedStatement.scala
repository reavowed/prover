package net.prover.model.expressions

import net.prover.model.entries.StatementDefinition

case class DefinedStatement(
    components: Seq[Expression],
    definition: StatementDefinition)(
    val scopedBoundVariableNames: Seq[String])
 extends Statement with DefinedExpression[Statement]
{
  override def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case DefinedStatement(otherComponents, `definition`) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def updateComponents(newComponents: Seq[Expression]): DefinedStatement = {
    DefinedStatement(newComponents, definition)(scopedBoundVariableNames)
  }
  override def updateBoundVariableNames(newBoundVariableNames: Seq[String]): DefinedStatement = {
    DefinedStatement(components, definition)(newBoundVariableNames)
  }
}
