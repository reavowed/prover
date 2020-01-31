package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

import scala.reflect.ClassTag

@JsonSerialize(using = classOf[ExpressionVariableSerializer])
abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression with TypedExpression[ExpressionType] with Substitutions.Lenses[ExpressionType] { this: ExpressionType =>
  def name: String
  def arguments: Seq[Term]
  val arity: Int = arguments.length

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newArguments: Seq[Term]): ExpressionType

  override def structuralComplexity: Int = arguments.map(_.structuralComplexity).sum + 1
  override def definitionalComplexity: Int = arguments.map(_.definitionalComplexity).sum + 1

  override def definitionUsages: DefinitionUsages = arguments.map(_.definitionUsages).foldTogether
  override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType = {
    update(arguments.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)))
  }
  override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType] = {
    arguments.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption.map(update)
  }
  override def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): ExpressionType = {
    update(arguments.map(_.replaceDefinition(oldDefinition, newDefinition)))
  }

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

  override def requiredSubstitutions: Substitutions.Required = {
    arguments.requiredSubstitutions ++ requiredSubstitutionsLens.set(Seq((name, arity)))(Substitutions.Required.empty)
  }
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Substitutions.Possible] = {
    if (other.isRuntimeInstance[ExpressionType]) {
      substitutionsLens.get(substitutions.stripApplications()).get(name) match {
        case Some((`arity`, applicative)) =>
          applicative.calculateArguments(other, Map.empty, 0, externalDepth).flatMap { otherArguments =>
            (0 until arity).foldLeft(Option(substitutions)) { case (substitutionOptions, index) =>
              substitutionOptions.flatMap { substitutionsSoFar =>
                otherArguments.get(index).map { otherArgument =>
                  arguments(index).calculateSubstitutions(otherArgument, substitutionsSoFar, internalDepth, externalDepth)
                }.getOrElse(Some(substitutionsSoFar))
              }
            }
          }
        case Some((otherArity, _)) =>
          None
        case None =>
          substitutions
            .updateAdd(
              name,
              arity,
              (arguments, other.asInstanceOf[ExpressionType], internalDepth),
              possibleSubstitutionsApplicationsLens)
            .flatMap(_.clearApplicationsWherePossible(externalDepth))
      }
    } else None
  }
  override def applySubstitutions(substitutions: Substitutions, internalDepth: Int, externalDepth: Int): Option[ExpressionType] = {
    for {
      (arity, predicate) <- substitutionsLens.get(substitutions).get(name)
      if arity == this.arity
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

  override def toString: String = name + (if (arguments.nonEmpty) "(" + arguments.map(_.toString).mkString(", ") + ")" else "")
  override def serialized: String = (if (arguments.nonEmpty) "with (" + arguments.map(_.serialized).mkString(" ") + ") " else "") + name
  override def serializedForHash: String = (if (arguments.nonEmpty) "with (" + arguments.map(_.serializedForHash).mkString(" ") + ") " else "") + name
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
    gen.writeStartObject(value)
    gen.writeObjectField(value.name, value.arguments)
    gen.writeEndObject()
  }
}

