package net.prover.model.expressions

import monocle.Lens
import net.prover.model.Substitutions

import scala.collection.immutable.Nil
import scala.reflect.ClassTag

abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression {
  def name: String
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]
  def increaseDepth(additionalDepth: Int): ExpressionType

  override def specify(targetArguments: Seq[Term]) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    increaseDepth(-1)
  }
  def specifyWithSubstitutions(targetArguments: Seq[Term], substitutions: Substitutions) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    Some(increaseDepth(-1))
  }

  override def requiredSubstitutions = requiredSubstitutionsLens.set(Seq(name))(Substitutions.Required.empty)
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions
  ): Seq[Substitutions] = {
    other match {
      case _ if other.isRuntimeInstance[ExpressionType] && other.depth == substitutions.depth =>
        substitutions.update(name, other.asInstanceOf[ExpressionType], substitutionsLens, 0).toSeq
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