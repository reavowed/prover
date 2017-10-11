package net.prover.model.expressions

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

import scala.collection.immutable.Nil

trait DefinedExpression[ExpressionType <: Expression] extends Expression {
  def components: Seq[Expression]
  def scopedBoundVariableNames: Seq[String]
  def definition: ExpressionDefinition

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newComponents: Seq[Expression], newDepth: Int): ExpressionType

  override def specify(targetArguments: Seq[Term]): ExpressionType = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    update(components.map(_.specify(targetArguments)), depth - 1)
  }
  def specifyWithSubstitutions(targetArguments: Seq[Term], substitutions: Substitutions) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    components.map(_.specifyWithSubstitutions(targetArguments, substitutions)).traverseOption.map(update(_, depth - 1))
  }
  override def increaseDepth(additionalDepth: Int): ExpressionType = {
    update(components.map(_.increaseDepth(additionalDepth)), depth + additionalDepth)
  }

  override def requiredSubstitutions = components.requiredSubstitutions
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    getMatch(other)
      .map(components.calculateSubstitutions(_, substitutions))
      .getOrElse(Nil)
  }
  override def applySubstitutions(substitutions: Substitutions): Option[ExpressionType] = {
    components.applySubstitutions(substitutions).map(update(_, depth + substitutions.depth))
  }

  override def calculateApplicatives(baseArguments: Seq[Term], substitutions: Substitutions): Seq[(ExpressionType, Substitutions)] = {
    components.calculateApplicatives(baseArguments, substitutions)
      .map(_.mapLeft(update(_, substitutions.depth + 1)))
  }
  override def makeApplicative(names: Seq[String]): Option[ExpressionType] = {
    if (scopedBoundVariableNames.isEmpty)
      components.map(_.makeApplicative(names)).traverseOption.map(update(_, depth + 1))
    else
      None
  }

  override def findComponentPath(other: Expression): Option[Seq[Int]] = {
    super.findComponentPath(other) orElse
      components.zipWithIndex.mapFind { case (subcomponent, index) =>
        subcomponent.findComponentPath(other).map(index +: _)
      }
  }

  override def toString: String = {
    definition.format(scopedBoundVariableNames ++ components.map(_.safeToString))
  }
  override def safeToString: String = {
    definition.format.safe(scopedBoundVariableNames ++ components.map(_.safeToString))
  }
  override def serialized: String = (Seq(definition.symbol) ++ scopedBoundVariableNames ++ components.map(_.serialized)).mkString(" ")
}

object DefinedExpression {
  def unapply(expression: DefinedExpression[_]): Option[(ExpressionDefinition, Seq[Expression])] = expression match {
    case definedExpression: DefinedExpression[_] =>
      Some((definedExpression.definition, definedExpression.components))
    case _ =>
      None
  }
}