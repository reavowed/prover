package net.prover.model.expressions

import net.prover.model.Substitutions
import net.prover.model.entries.TermDefinition

case class DefinedTerm(
    components: Seq[Expression],
    definition: TermDefinition)
  extends Term with DefinedExpression[Objectable, Term]
{
  def format = definition.format
  def symbol = definition.symbol
  def defaultVariables = definition.defaultVariables
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

  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    components.calculateApplicatives(arguments, substitutions).map(_.mapLeft(DefinedFunction(_, definition, 1)))
  }
  def updateApplicable(newComponents: Seq[Expression], depth: Int) = ???

  override def increaseDepth(additionalDepth: Int) = {
    DefinedFunction(components.map(_.increaseDepth(additionalDepth)), definition, additionalDepth)
  }
}
