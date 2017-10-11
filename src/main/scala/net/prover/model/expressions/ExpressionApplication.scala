package net.prover.model.expressions

import monocle.Lens
import net.prover.model._

import scala.reflect.ClassTag

abstract class ExpressionApplication[ExpressionType <: Expression : ClassTag] extends Expression {
  def variableName: String
  def arguments: Seq[Term]
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]

  def update(newArguments: Seq[Term], newDepth: Int): ExpressionType

  def specify(targetArguments: Seq[Term]) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    update(arguments.map(_.specify(targetArguments)), depth - 1)
  }
  def specifyWithSubstitutions(targetArguments: Seq[Term], substitutions: Substitutions) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    arguments.map(_.specifyWithSubstitutions(targetArguments, substitutions)).traverseOption.map(update(_, depth - 1))
  }
  def increaseDepth(additionalDepth: Int) = {
    update(arguments.map(_.increaseDepth(additionalDepth)), depth + additionalDepth)
  }

  override def requiredSubstitutions = requiredSubstitutionsLens.set(Seq(variableName))(Substitutions.Required.empty)
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
      result <- predicate.increaseDepth(depth).specifyWithSubstitutions(arguments, substitutions)
    } yield result.asInstanceOf[ExpressionType]
  }

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions
  ): Seq[(ExpressionType, Substitutions)] = {
    arguments.calculateApplicatives(baseArguments, substitutions).map(_.mapLeft { newArguments =>
      update(newArguments.map(_.asInstanceOf[Term]), substitutions.depth + 1)
    })
  }
  override def makeApplicative(names: Seq[String]): Option[ExpressionType] = None

  override def toString = s"$variableName(${arguments.map(_.toString).mkString(", ")})"
  override def safeToString = toString
  override def serialized = s"with (${arguments.map(_.serialized).mkString(", ")}) $variableName"
}

object ExpressionApplication {
  def unapply(expression: Expression): Option[(String, Seq[Term])] = expression match {
    case expressionApplication: ExpressionApplication[_] =>
      Some((expressionApplication.variableName, expressionApplication.arguments))
    case _ =>
      None
  }
}