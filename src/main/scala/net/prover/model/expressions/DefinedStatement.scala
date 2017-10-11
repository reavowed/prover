package net.prover.model.expressions

import net.prover.model.entries.StatementDefinition

case class DefinedStatement(
    components: Seq[Expression],
    definition: StatementDefinition,
    depth: Int)(
    val scopedBoundVariableNames: Seq[String])
 extends Statement with DefinedExpression[Statement]
{
  override def getMatch(other: Expression): Option[(Seq[Expression])] = other match {
    case DefinedStatement(otherComponents, `definition`, _) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def update(newComponents: Seq[Expression], newDepth: Int) = {
    DefinedStatement(newComponents, definition, newDepth)(scopedBoundVariableNames)
  }
}
