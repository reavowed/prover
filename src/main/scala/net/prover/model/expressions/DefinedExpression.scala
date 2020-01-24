package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

import scala.collection.immutable.Nil

@JsonSerialize(using = classOf[DefinedExpressionSerializer])
trait DefinedExpression[ExpressionType <: Expression] extends Expression with TypedExpression[ExpressionType] {
  def components: Seq[Expression]
  def scopedBoundVariableNames: Seq[String]
  def definition: ExpressionDefinition

  def getMatch(other: Expression): Option[Seq[Expression]]
  def updateComponents(newComponents: Seq[Expression]): ExpressionType
  def updateBoundVariableNames(newBoundVariableNames: Seq[String]): ExpressionType

  override def structuralComplexity: Int = {
    components.map(_.structuralComplexity).sum + 1
  }
  override def definitionalComplexity: Int = {
    components.map(_.definitionalComplexity).sum + definition.complexity
  }

  override def getTerms(depth: Int): Seq[(Term, ExpressionType, Seq[Int])] = {
    @scala.annotation.tailrec
    def helper(previous: Seq[Expression], next: Seq[Expression], acc: Seq[(Term, ExpressionType, Seq[Int])]): Seq[(Term, ExpressionType, Seq[Int])] = {
      next match {
        case current +: more =>
          helper(
            previous :+ current,
            more,
            acc ++ current.getTerms(definition.increaseDepth(depth)).map { case (term, function, path) => (term, updateComponents((previous :+ function) ++ more), previous.length +: path)})
        case _ =>
          acc
      }
    }
    if (scopedBoundVariableNames.isEmpty)
      helper(Nil, components, Nil)
    else
      Nil
  }
  override def definitionUsages: DefinitionUsages = components.map(_.definitionUsages).foldTogether.addUsage(definition)

  override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType = {
    updateComponents(components.map(_.insertExternalParameters(numberOfParametersToInsert, definition.increaseDepth(internalDepth))))
  }
  override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType] = {
    components
      .map(_.removeExternalParameters(numberOfParametersToRemove, definition.increaseDepth(internalDepth)))
      .traverseOption
      .map(updateComponents)
  }

  override def specify(
    targetArguments: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    components.map(_.specify(targetArguments, definition.increaseDepth(internalDepth), externalDepth)).traverseOption.map(updateComponents)
  }
  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    components
      .map(_.specifyWithSubstitutions(targetArguments, substitutions, definition.increaseDepth(internalDepth), previousInternalDepth, externalDepth)).traverseOption
      .map(updateComponents)
  }

  override def requiredSubstitutions = components.requiredSubstitutions
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Substitutions.Possible] = {
    getMatch(other)
      .flatMap(components.calculateSubstitutions(_, substitutions, definition.increaseDepth(internalDepth), externalDepth))
  }
  override def applySubstitutions(
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    components.applySubstitutions(substitutions, definition.increaseDepth(internalDepth), externalDepth).map(updateComponents)
  }
  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(ExpressionType, Substitutions.Possible)] = {
    components.calculateApplicatives(baseArguments, substitutions, definition.increaseDepth(internalDepth), previousInternalDepth, externalDepth)
      .map(_.mapLeft(updateComponents))
  }
  override def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[Map[Int, Term]] = {
    getMatch(target).flatMap(targetComponents => components.calculateArguments(targetComponents, argumentsSoFar, definition.increaseDepth(internalDepth), externalDepth))
  }

  override def renameBoundVariable(newName: String, index: Int, path: Seq[Int]): Option[ExpressionType] = {
    path match {
      case Nil =>
        if (scopedBoundVariableNames.lift(index).nonEmpty)
          Some(updateBoundVariableNames(scopedBoundVariableNames.updated(index, newName)))
        else
          None
      case head +: tail =>
        components.lift(head).flatMap(_.renameBoundVariable(newName, index, tail)).map(e => updateComponents(components.updated(head, e)))
    }
  }

  override def findComponentPath(other: Expression): Option[Seq[Int]] = {
    super.findComponentPath(other) orElse
      components.zipWithIndex.mapFind { case (subcomponent, index) =>
        subcomponent.findComponentPath(other).map(index +: _)
      }
  }

  override def toString: String = {
    definition.format.formatText(scopedBoundVariableNames ++ components.map(_.safeToString), parentRequiresBrackets = false)
  }
  override def safeToString: String = {
    definition.format.formatText(scopedBoundVariableNames ++ components.map(_.safeToString), parentRequiresBrackets = true)
  }
  override def serialized: String = (Seq(definition.symbol) ++ scopedBoundVariableNames ++ components.map(_.serialized)).mkString(" ")
  override def serializedForHash: String = (Seq(definition.symbol) ++ components.map(_.serializedForHash)).mkString(" ")
}

private class DefinedExpressionSerializer extends JsonSerializer[DefinedExpression[_]] {
  override def serialize(value: DefinedExpression[_], gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartArray(value.components.length + value.scopedBoundVariableNames.length + 1)
    gen.writeString(value.definition.symbol)
    value.scopedBoundVariableNames.foreach(gen.writeString)
    value.components.foreach(gen.writeObject)
    gen.writeEndArray()
  }
}
