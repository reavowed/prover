package net.prover.model.components

import net.prover.model.Substitutions
import net.prover.model.entries.StatementDefinition

case class DefinedStatement(
    subcomponents: Seq[Component],
    definition: StatementDefinition)(
    val scopedBoundVariableNames: Seq[String])
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
    copy(subcomponents = newSubcomponents)(scopedBoundVariableNames)
  }

  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    subcomponents.foldLeft(Seq((Seq.empty[Applicative[Component]], substitutions))) { case (predicatesAndSubstitutionsSoFar, subcomponent) =>
      for {
        (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
        (predicate, newSubstitutions) <- subcomponent.calculateApplicatives(
          argument,
          substitutionsSoFar,
          boundVariableCount + scopedBoundVariableNames.length)
      } yield (predicatesSoFar :+ predicate, newSubstitutions)
    }.map(_.mapLeft(components => Predicate.Defined(definition, components)(scopedBoundVariableNames)))
  }

  override def makeApplicative(argument: Term) = {
    subcomponents.map(_.makeApplicative(argument)).traverseOption.map(update)
  }
}
