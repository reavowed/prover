package net.prover.model.expressions

import monocle.Lens
import net.prover.model._

import scala.collection.immutable.Nil
import scala.reflect.ClassTag

abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression {
  def name: String
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]
  def setDepth(newDepth: Int): ExpressionType

  override def increaseDepth(difference: Int, insertionPoint: Int): ExpressionType = {
    setDepth(depth + difference)
  }
  override def reduceDepth(difference: Int, insertionPoint: Int): Option[ExpressionType] = {
    if (depth >= difference)
      Some(setDepth(depth - difference))
    else
      None
  }
  override def specify(targetArguments: ArgumentList) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    setDepth(depth - 1)
  }
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions
  ) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    Some(setDepth(depth + targetArguments.depth - 1))
  }

  override def requiredSubstitutions = requiredSubstitutionsLens.set(Seq(name))(Substitutions.Required.empty)
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Seq[Substitutions] = {
    other match {
      case _ if other.isRuntimeInstance[ExpressionType] && other.depth >= depth + substitutions.depth =>
        (for {
          reducedOther <- other.reduceDepth(depth, substitutions.depth)
          if applicativeHints.forall { hint =>
            val applicatives = other.calculateApplicatives(hint._2, Substitutions.emptyWithDepth(substitutions.depth))
            substitutionsLens.get(hint._1).get(name).forall { s =>
              applicatives.exists(_._1 == s)
            }
          }
          result <- substitutions.update(name, reducedOther.asInstanceOf[ExpressionType], substitutionsLens, 0)
        } yield result).toSeq
      case _ =>
        Nil
    }
  }
  def applySubstitutions(substitutions: Substitutions): Option[ExpressionType] = {
    substitutionsLens.get(substitutions).get(name).map(_.increaseDepth(depth, substitutions.depth).asInstanceOf[ExpressionType])
  }

  def calculateApplicatives(
    baseArguments: ArgumentList,
    substitutions: Substitutions
  ): Seq[(ExpressionType, Substitutions)] = {
    Seq((setDepth(depth - baseArguments.depth + 1), substitutions))
  }

  override def condense(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ) = {
    super.condense(other, thisSubstitutions, otherSubstitutions, applicativeHints, structuralHints) ++
      applicativeHints
        .foldProduct { case (hintSubstitutions, hintArgumentList) =>
          for {
            hintApplicative <- substitutionsLens.get(hintSubstitutions).get(name).toSeq
            newHintSubstitutions <- other.calculateSubstitutions(
              hintApplicative.increaseDepth(depth, thisSubstitutions.depth + 1),
              Substitutions.emptyWithDepth(thisSubstitutions.depth + 1),
              Nil,
              Nil)
          } yield newHintSubstitutions -> hintArgumentList
        }
        .filter(_.nonEmpty)
        .map((thisSubstitutions, otherSubstitutions, _, Nil))
  }

  def matchesStructure(other: Expression): Boolean = other.isRuntimeInstance[ExpressionType]
}

object ExpressionVariable {
  def unapply(expression: Expression): Option[String] = expression match {
    case expressionVariable: ExpressionVariable[_] =>
      Some(expressionVariable.name)
    case _ =>
      None
  }
}