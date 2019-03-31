package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import monocle.Lens
import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

import scala.reflect.ClassTag

@JsonSerialize(using = classOf[ExpressionApplicationSerializer])
abstract class ExpressionApplication[ExpressionType <: Expression : ClassTag] extends Expression with TypedExpression[ExpressionType] {
  def variableName: String
  def arguments: Seq[Term]
  def substitutionsLens: Lens[Substitutions, Map[(String, Int), ExpressionType]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[(String, Int)]]

  def update(newArguments: Seq[Term]): ExpressionType

  override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
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
    arguments.requiredSubstitutions ++ requiredSubstitutionsLens.set(Seq((variableName, arguments.length)))(Substitutions.Required.empty)
  }
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ) = {
    if (other.isRuntimeInstance[ExpressionType]) {
      for {
        (applicative, applicativeSubstitutions) <- other.calculateApplicatives(arguments, substitutions, 0, internalDepth, externalDepth)
        substitutionsWithApplicative <- applicativeSubstitutions.update(
          (variableName, arguments.length),
          applicative.asInstanceOf[ExpressionType],
          substitutionsLens,
          1)
      } yield substitutionsWithApplicative
    } else Nil
  }

  override def applySubstitutions(substitutions: Substitutions, internalDepth: Int, externalDepth: Int): Option[ExpressionType] = {
    for {
      predicate <- substitutionsLens.get(substitutions).get((variableName, arguments.length))
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

  override def toString = s"$variableName(${arguments.map(_.toString).mkString(", ")})"
  override def safeToString = toString
  override def serialized = s"with (${arguments.map(_.serialized).mkString(" ")}) $variableName"
  override def serializedForHash = s"with (${arguments.map(_.serializedForHash).mkString(" ")}) $variableName"
}

object ExpressionApplication {
  def unapply(expression: Expression): Option[(String, Seq[Term])] = expression match {
    case expressionApplication: ExpressionApplication[_] =>
      Some((expressionApplication.variableName, expressionApplication.arguments))
    case _ =>
      None
  }
}

private class ExpressionApplicationSerializer extends JsonSerializer[ExpressionApplication[_]] {
  override def serialize(value: ExpressionApplication[_], gen: JsonGenerator, serializers: SerializerProvider) = {
    gen.writeStartObject(value)
    gen.writeObjectField(value.variableName, value.arguments)
    gen.writeEndObject()
  }
}
