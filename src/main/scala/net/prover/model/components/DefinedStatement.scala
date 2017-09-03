package net.prover.model.components

import net.prover.model.Substitutions
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

  override def calculateApplicatives(argument: Term, substitutions: Substitutions): Seq[(Predicate, Substitutions)] = {
    super.calculateApplicatives(argument, substitutions) ++
      subcomponents.foldLeft(Seq((Seq.empty[Applicative[Component]], substitutions))) { case (predicatesAndSubstitutionsSoFar, subcomponent) =>
        for {
          (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
          (predicate, newSubstitutions) <- subcomponent.calculateApplicatives(argument, substitutionsSoFar)
        } yield (predicatesSoFar :+ predicate, newSubstitutions)
      }.map(_.mapLeft(Predicate.Defined(_, definition)))
        .filter(!_._1.isConstant)
  }
}
