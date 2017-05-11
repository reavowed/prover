package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

case class DistinctVariables(private val map: Map[TermVariable, Variables]) extends JsonSerializable.Base {
  def ++(other: DistinctVariables): DistinctVariables = {
    other.map.foldLeft(map) { case (currentMap, (termVariable, variables)) =>
      val updatedVariables = currentMap.get(termVariable) match {
        case Some(otherVariables) =>
          otherVariables ++ variables
        case None =>
          variables
      }
      currentMap.updated(termVariable, updatedVariables)
    }
  }

  def get(termVariable: TermVariable): Variables = {
    map.getOrElse(termVariable, Variables.empty)
  }

  def areDistinct(termVariable: TermVariable, statementVariable: StatementVariable): Boolean = {
    map.get(termVariable).exists(_.statementVariables.contains(statementVariable))
  }

  def areDistinct(termVariable: TermVariable, otherTermVariable: TermVariable): Boolean = {
    map.get(termVariable).exists(_.termVariables.contains(otherTermVariable)) ||
      map.get(otherTermVariable).exists(_.termVariables.contains(termVariable))
  }

  def restrictTo(activeVariables: Variables): DistinctVariables = {
    map
      .mapValues(_ intersect activeVariables)
      .filter { case (termVariable, variables) =>
        activeVariables.termVariables.contains(termVariable) && variables.nonEmpty
      }
  }

  def applySubstitutions(substitutions: Substitutions): Option[DistinctVariables] = {
    map
      .map { case (termVariable, variables) =>
        substituteSingleVariableCondition(termVariable, variables, substitutions)
      }
      .traverseOption
      .map(_.toMap)
  }

  private def substituteSingleVariableCondition(
    termVariable: TermVariable,
    variables: Variables,
    substitutions: Substitutions
  ): Option[(TermVariable, Variables)] = {
    for {
      updatedTermVariable <- termVariable.applySubstitutions(substitutions).flatMap(Term.optionAsVariable)
      updatedStatementVariables <- variables.statementVariables
        .map(_.applySubstitutions(substitutions).map(_.getPotentiallyIntersectingVariables(updatedTermVariable)))
        .traverseOption
      updatedTermVariables <- variables.termVariables
        .map(_.applySubstitutions(substitutions).map(_.getPotentiallyIntersectingVariables(updatedTermVariable)))
        .traverseOption
      updatedOtherVariables = (updatedStatementVariables ++ updatedTermVariables).reduce(_ ++ _)
      if !updatedOtherVariables.termVariables.contains(updatedTermVariable)
    } yield {
      updatedTermVariable -> updatedOtherVariables
    }
  }

  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartArray()
    map.foreach { case (termVariable, variables) =>
      gen.writeStartObject()
      gen.writeObjectField("termVariable", termVariable)
      gen.writeObjectField("variables", variables.statementVariables ++ variables.termVariables)
      gen.writeEndObject()
    }
    gen.writeEndArray()
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

object DistinctVariables {
  val empty: DistinctVariables = new DistinctVariables(Map.empty)

  def apply(tuple: (TermVariable, StatementVariable)): DistinctVariables = {
    Map(tuple._1 -> Variables(tuple._2))
  }

  def apply(tuple: (TermVariable, TermVariable))(implicit dummyImplicit: DummyImplicit): DistinctVariables = {
    Map(tuple._1 -> Variables(tuple._2))
  }

  def apply(tuple: (TermVariable, Variables))(implicit dummyImplicit1: DummyImplicit, dummyImplicit2: DummyImplicit): DistinctVariables = {
    Map(tuple)
  }

  def byStatements(termVariables: Set[TermVariable], statements: Seq[Statement]): Option[DistinctVariables] = {
    termVariables
      .map { termVariable =>
        val variables = statements.map(_.getPotentiallyIntersectingVariables(termVariable)).reduceOption(_ ++ _)
          .getOrElse(Variables.empty)
        if (variables.termVariables.contains(termVariable))
          None
        else
          Some(termVariable -> variables)
      }
      .traverseOption
      .map(_.toMap.filter(_._2.nonEmpty))
  }

  private implicit def fromMap(map: Map[TermVariable, Variables]): DistinctVariables = new DistinctVariables(map)
}
