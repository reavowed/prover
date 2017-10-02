package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.entries.StatementDefinition

case class DefinedStatement(
    components: Seq[Expression],
    definition: StatementDefinition)(
    val scopedBoundVariableNames: Seq[String])
 extends Statement with DefinedExpression[Statement]
{
  def format = definition.format
  def symbol = definition.symbol

  override def getMatch(other: Expression): Option[(Seq[Expression])] = other match {
    case DefinedStatement(otherSubcomponents, `definition`) =>
      Some(otherSubcomponents)
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Expression]): Statement = {
    copy(components = newSubcomponents)(scopedBoundVariableNames)
  }

  override def calculateApplicatives(arguments: Seq[Term], substitutions: Substitutions, boundVariableCount: Int) = {
    components.foldLeft(Seq((Seq.empty[ExpressionFunction[Expression]], substitutions))) { case (predicatesAndSubstitutionsSoFar, subcomponent) =>
      for {
        (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
        (predicate, newSubstitutions) <- subcomponent.calculateApplicatives(
          arguments,
          substitutionsSoFar,
          boundVariableCount + scopedBoundVariableNames.length)
      } yield (predicatesSoFar :+ predicate, newSubstitutions)
    }.map(_.mapLeft(components => DefinedPredicate(definition, components)(scopedBoundVariableNames)))
  }

  override def makeApplicative(argument: Term) = {
    components.map(_.makeApplicative(argument)).traverseOption.map(update)
  }
}
