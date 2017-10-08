package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.entries.StatementDefinition

case class DefinedStatement(
    components: Seq[Expression],
    definition: StatementDefinition)(
    val scopedBoundVariableNames: Seq[String])
 extends Statement with DefinedExpression[Assertable, Statement]
{
  def format = definition.format
  def symbol = definition.symbol
  def defaultVariables = definition.defaultVariables

  override def getMatch(other: Expression): Option[(Seq[Expression])] = other match {
    case DefinedStatement(otherSubcomponents, `definition`) =>
      Some(otherSubcomponents)
    case DefinedPredicate(otherSubcomponents, `definition`, _) =>
      Some(otherSubcomponents)
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Expression]): Statement = {
    copy(components = newSubcomponents)(scopedBoundVariableNames)
  }
  def updateApplicable(newComponents: Seq[Expression], depth: Int): Predicate = {
    DefinedPredicate(newComponents.map(_.asInstanceOf[ExpressionFunction[Expression]]), definition, depth)(scopedBoundVariableNames)
  }

  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    components.calculateApplicatives(arguments, substitutions).map(_.mapLeft(DefinedPredicate(_, definition, 1)(scopedBoundVariableNames)))
  }

  override def makeApplicative = {
    if (scopedBoundVariableNames.isEmpty)
      components.map(_.makeApplicative).traverseOption.map(x => DefinedPredicate(x, definition, 0)(Nil))
    else
      None
  }

  override def increaseDepth(additionalDepth: Int) = {
    DefinedPredicate(
      components.map(_.increaseDepth(additionalDepth)),
      definition,
      additionalDepth)(
      scopedBoundVariableNames)
  }
}
