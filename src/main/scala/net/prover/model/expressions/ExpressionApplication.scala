package net.prover.model.expressions

import monocle.Lens
import net.prover.model._

import scala.reflect.ClassTag

abstract class ExpressionApplication[ExpressionType <: Expression : ClassTag] extends Expression {
  def variableName: String
  def arguments: ArgumentList
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]

  override def depth = arguments.depth

  def update(newArguments: ArgumentList): ExpressionType

  def specify(targetArguments: ArgumentList) = {
    update(arguments.specify(targetArguments))
  }
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions,
    outerDepth: Int
  ) = {
    arguments.specifyWithSubstitutions(targetArguments, substitutions, outerDepth).map(update)
  }
  def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    update(arguments.increaseDepth(additionalDepth, insertionPoint))
  }
  override def reduceDepth(difference: Int, insertionPoint: Int): Option[ExpressionType] = {
    if (depth >= difference)
      arguments.reduceDepth(difference, insertionPoint).map(update)
    else
      None
  }

  override def requiredSubstitutions = {
    arguments.requiredSubstitutions ++ requiredSubstitutionsLens.set(Seq(variableName))(Substitutions.Required.empty)
  }
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case otherWithMatchingType if otherWithMatchingType.isRuntimeInstance[ExpressionType] =>
        otherWithMatchingType.asInstanceOf[ExpressionType]
          .calculateApplicatives(arguments, substitutions)
          .flatMap { case (result, newSubstitutions) =>
            newSubstitutions.update(variableName, result.asInstanceOf[ExpressionType], substitutionsLens, 1)
          }
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[ExpressionType] = {
    for {
      predicate <- substitutionsLens.get(substitutions).get(variableName)
      result <- predicate.specifyWithSubstitutions(arguments, substitutions, depth)
    } yield result.asInstanceOf[ExpressionType]
  }
  override def calculateApplicatives(
    baseArguments: ArgumentList,
    substitutions: Substitutions
  ): Seq[(ExpressionType, Substitutions)] = {
    arguments.calculateApplicatives(baseArguments, substitutions).map(_.mapLeft(update))
  }

  override def toString = s"$variableName(${arguments.terms.map(_.toString).mkString(", ")})"
  override def safeToString = toString
  override def serialized = s"with (${arguments.terms.map(_.serialized).mkString(", ")}) $variableName"
}

object ExpressionApplication {
  def unapply(expression: Expression): Option[(String, Seq[Term])] = expression match {
    case expressionApplication: ExpressionApplication[_] =>
      Some((expressionApplication.variableName, expressionApplication.arguments.terms))
    case _ =>
      None
  }
}