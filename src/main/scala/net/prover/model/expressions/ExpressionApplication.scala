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
  val arity: Int = arguments.length
  def variableName: String
  def arguments: Seq[Term]
  def substitutionsLens: Lens[Substitutions, Map[(String, Int), ExpressionType]]
  def possibleSubstitutionsLens: Lens[Substitutions.Possible, Map[(String, Int), ExpressionType]]
  def possibleSubstitutionsApplicationsLens: Lens[Substitutions.Possible, Map[(String, Int), Seq[(Seq[Term], ExpressionType, Int)]]]
  def requiredSubstitutionsLens: Lens[Substitutions.Required, Seq[(String, Int)]]

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newArguments: Seq[Term]): ExpressionType

  override def structuralComplexity: Int = arguments.map(_.structuralComplexity).sum + 1
  override def definitionalComplexity: Int = arguments.map(_.definitionalComplexity).sum + 1

  override def getTerms(depth: Int): Seq[(Term, ExpressionType, Seq[Int])] = {
    def helper(previous: Seq[Term], next: Seq[Term], acc: Seq[(Term, ExpressionType, Seq[Int])]): Seq[(Term, ExpressionType, Seq[Int])] = {
      next match {
        case current +: more =>
          helper(
            previous :+ current,
            more,
            acc ++ current.getTerms(depth).map { case (term, function, path) => (term, update((previous :+ function) ++ more), previous.length +: path)})
        case _ =>
          acc
      }
    }
    helper(Nil, arguments, Nil)
  }
  override def definitionUsages: DefinitionUsages = DefinitionUsages.empty
  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType = {
    update(arguments.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)))
  }
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType] = {
    arguments.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption.map(update)
  }
  override def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): ExpressionType = {
    update(arguments.map(_.replaceDefinition(oldDefinition, newDefinition)))
  }

  def specify(
    targetArguments: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    arguments.map(_.specify(targetArguments, internalDepth, externalDepth)).traverseOption.map(update)
  }
  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    arguments.map(_.specifyWithSubstitutions(targetArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)).traverseOption.map(update)
  }

  override def requiredSubstitutions = {
    arguments.requiredSubstitutions ++ requiredSubstitutionsLens.set(Seq((variableName, arity)))(Substitutions.Required.empty)
  }
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    externalDepth: Int
  ) = {
    if (other.isRuntimeInstance[ExpressionType]) {
      substitutionsLens.get(substitutions.stripApplications()).get((variableName, arity)) match {
        case Some(applicative) =>
          applicative.calculateArguments(other, Map.empty, 0, externalDepth).flatMap { otherArguments =>
            (0 until arity).foldLeft(Option(substitutions)) { case (substitutionOptions, index) =>
              substitutionOptions.flatMap { substitutionsSoFar =>
                otherArguments.get(index).map { otherArgument =>
                  arguments(index).calculateSubstitutions(otherArgument, substitutionsSoFar, internalDepth, externalDepth)
                }.getOrElse(Some(substitutionsSoFar))
              }
            }
          }
        case None =>
          substitutions
            .updateAdd(
              (variableName, arity),
              (arguments, other.asInstanceOf[ExpressionType], internalDepth),
              possibleSubstitutionsApplicationsLens)
            .clearApplicationsWherePossible(externalDepth)
      }
    } else None
  }

  override def applySubstitutions(substitutions: Substitutions, internalDepth: Int, externalDepth: Int): Option[ExpressionType] = {
    for {
      predicate <- substitutionsLens.get(substitutions).get((variableName, arity))
      result <- predicate.specifyWithSubstitutions(arguments, substitutions, 0, internalDepth, externalDepth)
    } yield result.asInstanceOf[ExpressionType]
  }

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(ExpressionType, Substitutions.Possible)] = {
    arguments.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
      .map(_.mapLeft(newArguments => update(newArguments.map(_.asInstanceOf[Term]))))
  }
  override def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[Map[Int, Term]] = {
    getMatch(target).flatMap(targetComponents => arguments.calculateArguments(targetComponents, argumentsSoFar, internalDepth, externalDepth))
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
