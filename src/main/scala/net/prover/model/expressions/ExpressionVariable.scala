package net.prover.model.expressions

import monocle.Lens
import net.prover.model.Substitutions

import scala.collection.immutable.Nil
import scala.reflect.ClassTag

abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression {
  def name: String
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]
  def setDepth(newDepth: Int): ExpressionType

  override def increaseDepth(difference: Int): ExpressionType = {
    setDepth(depth + difference)
  }
  override def reduceDepth(difference: Int): Option[ExpressionType] = {
    if (depth >= difference)
      Some(setDepth(depth - difference))
    else
      None
  }
  override def specify(targetArguments: Seq[Term]) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    setDepth(depth - 1)
  }
  def specifyWithSubstitutions(targetArguments: Seq[Term], substitutions: Substitutions) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    Some(setDepth(depth - 1))
  }

  override def requiredSubstitutions = requiredSubstitutionsLens.set(Seq(name))(Substitutions.Required.empty)
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions
  ): Seq[Substitutions] = {
    other match {
      case _ if other.isRuntimeInstance[ExpressionType] && other.depth == depth + substitutions.depth =>
        (for {
          reducedOther <- other.reduceDepth(depth)
          result <- substitutions.update(name, reducedOther.asInstanceOf[ExpressionType], substitutionsLens, 0)
        } yield result).toSeq
      case _ =>
        Nil
    }
  }
  def applySubstitutions(substitutions: Substitutions): Option[ExpressionType] = {
    substitutionsLens.get(substitutions).get(name).map(_.increaseDepth(depth).asInstanceOf[ExpressionType])
  }

  def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions
  ): Seq[(ExpressionType, Substitutions)] = {
    Seq((increaseDepth(substitutions.depth + 1 - depth), substitutions))
  }
}

object ExpressionVariable {
  def unapply(expression: Expression): Option[String] = expression match {
    case expressionVariable: ExpressionVariable[_] =>
      Some(expressionVariable.name)
    case _ =>
      None
  }
}