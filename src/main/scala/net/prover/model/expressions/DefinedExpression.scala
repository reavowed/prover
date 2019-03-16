package net.prover.model.expressions

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

import scala.collection.immutable.Nil

trait DefinedExpression[ExpressionType <: Expression] extends Expression with TypedExpression[ExpressionType] {
  def components: Seq[Expression]
  def scopedBoundVariableNames: Seq[String]
  def definition: ExpressionDefinition

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newComponents: Seq[Expression]): ExpressionType

  private def increaseDepth(internalDepth: Int) = if (scopedBoundVariableNames.nonEmpty) internalDepth + 1 else internalDepth

  override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType = {
    update(components.map(_.insertExternalParameters(numberOfParametersToInsert, increaseDepth(internalDepth))))
  }
  override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType] = {
    components
      .map(_.removeExternalParameters(numberOfParametersToRemove, increaseDepth(internalDepth)))
      .traverseOption
      .map(update)
  }

  override def specify(
    targetArguments: Seq[Term],
    internalDepth: Int,
    externalDepth: Int
  ): ExpressionType = {
    update(components.map(_.specify(targetArguments, increaseDepth(internalDepth), externalDepth)))
  }
  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    components
      .map(_.specifyWithSubstitutions(targetArguments, substitutions, increaseDepth(internalDepth), previousInternalDepth, externalDepth)).traverseOption
      .map(update)
  }

  override def requiredSubstitutions = components.requiredSubstitutions
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ) = {
    getMatch(other)
      .map(components.calculateSubstitutions(_, substitutions, increaseDepth(internalDepth), externalDepth))
      .getOrElse(Nil)
  }
  override def applySubstitutions(
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    components.applySubstitutions(substitutions, increaseDepth(internalDepth), externalDepth).map(update)
  }
  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Seq[(ExpressionType, Substitutions)] = {
    components.calculateApplicatives(baseArguments, substitutions, increaseDepth(internalDepth), previousInternalDepth, externalDepth)
      .map(_.mapLeft(update))
  }

  override def findComponentPath(other: Expression): Option[Seq[Int]] = {
    super.findComponentPath(other) orElse
      components.zipWithIndex.mapFind { case (subcomponent, index) =>
        subcomponent.findComponentPath(other).map(index +: _)
      }
  }

  override def toString: String = {
    definition.format.formatText(scopedBoundVariableNames ++ components.map(_.safeToString), safe = false)
  }
  override def safeToString: String = {
    definition.format.formatText(scopedBoundVariableNames ++ components.map(_.safeToString), safe = true)
  }
  override def serialized: String = (Seq(definition.symbol) ++ scopedBoundVariableNames ++ components.map(_.serialized)).mkString(" ")
  override def serializedForHash: String = (Seq(definition.symbol) ++ components.map(_.serializedForHash)).mkString(" ")
}

object DefinedExpression {
  def unapply(expression: DefinedExpression[_]): Option[(ExpressionDefinition, Seq[String], Seq[Expression])] = expression match {
    case definedExpression: DefinedExpression[_] =>
      Some((definedExpression.definition, definedExpression.scopedBoundVariableNames, definedExpression.components))
    case _ =>
      None
  }
}
