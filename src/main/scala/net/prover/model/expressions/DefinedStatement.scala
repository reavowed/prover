package net.prover.model.expressions

import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}

case class DefinedStatement(
    components: Seq[Expression],
    definition: StatementDefinition)(
    val boundVariableNames: Seq[String])
 extends Statement with DefinedExpression[Statement]
{
  override def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case DefinedStatement(otherComponents, `definition`) =>
      Some(otherComponents)
    case _ =>
      None
  }
  override def updateComponents(newComponents: Seq[Expression]): DefinedStatement = {
    DefinedStatement(newComponents, definition)(boundVariableNames)
  }
  override def updateBoundVariableNames(newBoundVariableNames: Seq[String]): DefinedStatement = {
    DefinedStatement(components, definition)(newBoundVariableNames)
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): DefinedStatement = {
    DefinedStatement(
      components.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      expressionDefinitionReplacements(definition).asInstanceOf[StatementDefinition]
    )(boundVariableNames)
  }
}
