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
    substitutions: Substitutions
  ) = {
    arguments.specifyWithSubstitutions(targetArguments, substitutions).map(update)
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
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ) = {
    if (other.isRuntimeInstance[ExpressionType]) {
      for {
        (applicative, applicativeSubstitutions) <- other.calculateApplicatives(arguments, substitutions)
        if structuralHints.forall { structuralHint =>
          substitutionsLens.get(structuralHint).get(variableName).forall { s =>
            s.matchesStructure(applicative)
          }
        }
        substitutionsWithApplicative <- applicativeSubstitutions.update(
          variableName,
          applicative.asInstanceOf[ExpressionType],
          substitutionsLens,
          1)
      } yield substitutionsWithApplicative
    } else Nil
  }

  override def applySubstitutions(substitutions: Substitutions): Option[ExpressionType] = {
    for {
      predicate <- substitutionsLens.get(substitutions).get(variableName)
      result <- predicate.specifyWithSubstitutions(arguments, substitutions)
    } yield result.asInstanceOf[ExpressionType]
  }
  override def calculateApplicatives(
    baseArguments: ArgumentList,
    substitutions: Substitutions
  ): Seq[(ExpressionType, Substitutions)] = {
    arguments.calculateApplicatives(baseArguments, substitutions).map(_.mapLeft(update))
  }

  override def condense(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ) = {
    val base = super.condense(other, thisSubstitutions, otherSubstitutions, applicativeHints, structuralHints)
    if (base.nonEmpty)
      base
    else for {
      predicate <- substitutionsLens.get(thisSubstitutions).get(variableName).toSeq
      predicateSubstitutions <- other.calculateSubstitutions(
        predicate.increaseDepth(depth, thisSubstitutions.depth),
        Substitutions.emptyWithDepth(thisSubstitutions.depth + 1),
        Nil,
        Nil)
    } yield (thisSubstitutions, otherSubstitutions, Seq((predicateSubstitutions, arguments)), Nil)
  }

  override protected def condenseReverse(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ) = {
    val base = super.condenseReverse(other, thisSubstitutions, otherSubstitutions)
    if (base.nonEmpty)
      base
    else
      for {
        (applicative, newSubstitutions) <- other.calculateApplicatives(arguments, Substitutions.empty)
        updatedSubstitutions <- newSubstitutions.update(
          variableName,
          applicative.asInstanceOf[ExpressionType],
          substitutionsLens,
          1)
      } yield (thisSubstitutions, otherSubstitutions, Nil, Seq(updatedSubstitutions))
  }

  def matchesStructure(other: Expression): Boolean = false // TODO: Probably these can match?

  override def toString = s"$variableName(${arguments.terms.map(_.toString).mkString(", ")})"
  override def safeToString = toString
  override def serialized = s"with (${arguments.terms.map(_.serialized).mkString(" ")}) $variableName"
}

object ExpressionApplication {
  def unapply(expression: Expression): Option[(String, Seq[Term])] = expression match {
    case expressionApplication: ExpressionApplication[_] =>
      Some((expressionApplication.variableName, expressionApplication.arguments.terms))
    case _ =>
      None
  }
}
