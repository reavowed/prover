package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import monocle.Lens
import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

import scala.collection.immutable.Nil
import scala.reflect.ClassTag

@JsonSerialize(using = classOf[ExpressionVariableSerializer])
abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression with TypedExpression[ExpressionType] { this: ExpressionType =>
  def name: String
  def substitutionsLens: Lens[Substitutions, Map[String, ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[String]]

  override def complexity: Int = 0
  override def definitionUsages: DefinitionUsages = DefinitionUsages.empty
  override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0) = this
  override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0) = Some(this)
  override def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): ExpressionType = this

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
  override def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[Map[Int, Term]] = {
    if (target == this)
      Some(argumentsSoFar)
    else
      None
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

private class ExpressionVariableSerializer extends JsonSerializer[ExpressionVariable[_]] {
  override def serialize(value: ExpressionVariable[_], gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(value.name)
  }
}
