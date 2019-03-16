package net.prover.model.expressions

import monocle.Lens
import net.prover.model._

import scala.collection.immutable.Nil
import scala.reflect.ClassTag

abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression with TypedExpression[ExpressionType] { this: ExpressionType =>
  def name: String
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]

  override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0) = this
  override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0) = Some(this)

  override def specify(
    targetArguments: Seq[Term],
    internalDepth: Int,
    externalDepth: Int
  ) = {
    this
  }
  override def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ) = {
    Some(this)
  }

  override def requiredSubstitutions = requiredSubstitutionsLens.set(Seq(name))(Substitutions.Required.empty)
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Seq[Substitutions] = {
    other match {
      case _  if other.isRuntimeInstance[ExpressionType] =>
        (for {
          reducedOther <- other.removeExternalParameters(internalDepth)
          result <- substitutions.update(name, reducedOther.asInstanceOf[ExpressionType], substitutionsLens, 0)
        } yield result).toSeq
      case _ =>
        Nil
    }
  }
  def applySubstitutions(
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    substitutionsLens.get(substitutions).get(name).map(_.insertExternalParameters(internalDepth).asInstanceOf[ExpressionType])
  }

  def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Seq[(ExpressionType, Substitutions)] = {
    Seq((this, substitutions))
  }

  override def toString: String = name
  override def serialized: String = name
  override def serializedForHash: String = name
}

object ExpressionVariable {
  def unapply(expression: Expression): Option[String] = expression match {
    case expressionVariable: ExpressionVariable[_] =>
      Some(expressionVariable.name)
    case _ =>
      None
  }
}
