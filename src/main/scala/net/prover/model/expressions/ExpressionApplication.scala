package net.prover.model.expressions

import monocle.Lens
import net.prover.model._

import scala.reflect.ClassTag

abstract class ExpressionApplication[ExpressionType <: Expression : ClassTag] extends Expression with TypedExpression[ExpressionType] {
  def variableName: String
  def arguments: Seq[Term]
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]

  def update(newArguments: Seq[Term]): ExpressionType

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType = {
    update(arguments.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)))
  }
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType] = {
    arguments.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption.map(update)
  }

  def specify(
    targetArguments: Seq[Term],
    internalDepth: Int,
    externalDepth: Int
  ) = {
    update(arguments.map(_.specify(targetArguments, internalDepth, externalDepth)))
  }
  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    arguments.map(_.specifyWithSubstitutions(targetArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)).traverseOption.map(update)
  }

  override def requiredSubstitutions = {
    arguments.requiredSubstitutions ++ requiredSubstitutionsLens.set(Seq(variableName))(Substitutions.Required.empty)
  }
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, Seq[Term])],
    structuralHints: Seq[Substitutions],
    internalDepth: Int,
    externalDepth: Int
  ) = {
    if (other.isRuntimeInstance[ExpressionType]) {
      for {
        (applicative, applicativeSubstitutions) <- other.calculateApplicatives(arguments, substitutions, 0, internalDepth, externalDepth)
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

  override def applySubstitutions(substitutions: Substitutions, internalDepth: Int, externalDepth: Int): Option[ExpressionType] = {
    for {
      predicate <- substitutionsLens.get(substitutions).get(variableName)
      result <- predicate.specifyWithSubstitutions(arguments, substitutions, 0, internalDepth, externalDepth)
    } yield result.asInstanceOf[ExpressionType]
  }

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Seq[(ExpressionType, Substitutions)] = {
    arguments.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
      .map(_.mapLeft(newArguments => update(newArguments.map(_.asInstanceOf[Term]))))
  }

  override def condense(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, Seq[Term])],
    structuralHints: Seq[Substitutions],
    internalDepth: Int,
    externalDepth: Int
  ) = {
    val base = super.condense(other, thisSubstitutions, otherSubstitutions, applicativeHints, structuralHints, internalDepth, externalDepth)
    if (base.nonEmpty)
      base
    else for {
      predicate <- substitutionsLens.get(thisSubstitutions).get(variableName).toSeq
      predicateSubstitutions <- other.calculateSubstitutions(
        predicate.insertExternalParameters(internalDepth),
        Substitutions.empty,
        Nil,
        Nil,
        internalDepth,
        externalDepth)
    } yield (thisSubstitutions, otherSubstitutions, Seq((predicateSubstitutions, arguments)), Nil)
  }

  override protected def condenseReverse(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ) = {
    val base = super.condenseReverse(other, thisSubstitutions, otherSubstitutions, internalDepth, externalDepth)
    if (base.nonEmpty)
      base
    else
      for {
        (applicative, newSubstitutions) <- other.calculateApplicatives(arguments, Substitutions.empty, 0, internalDepth, externalDepth)
        updatedSubstitutions <- newSubstitutions.update(
          variableName,
          applicative.asInstanceOf[ExpressionType],
          substitutionsLens,
          1)
      } yield (thisSubstitutions, otherSubstitutions, Nil, Seq(updatedSubstitutions))
  }

  def matchesStructure(other: Expression): Boolean = false // TODO: Probably these can match?

  override def toString = s"$variableName(${arguments.map(_.toString).mkString(", ")})"
  override def safeToString = toString
  override def serialized = s"with (${arguments.map(_.serialized).mkString(" ")}) $variableName"
}

object ExpressionApplication {
  def unapply(expression: Expression): Option[(String, Seq[Term])] = expression match {
    case expressionApplication: ExpressionApplication[_] =>
      Some((expressionApplication.variableName, expressionApplication.arguments))
    case _ =>
      None
  }
}
