package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

case class DistinctVariableRequirementViolationException(
    variable: TermVariable)
  extends Exception(
    s"Distinct variable requirement violated: $variable")

case class DistinctVariableRequirements(map: Map[TermVariable, Variables]) extends JsonSerializable.Base {
  def applyMatch(m: Match): DistinctVariableRequirements = {
    DistinctVariableRequirements(map.map { case (termVariable, Variables(statementVariables, termVariables)) =>
      val updatedTermVariable = Term.asVariable(termVariable.applyMatch(m))
      val updatedOtherVariables = (statementVariables.map(_.applyMatch(m).variables) ++ termVariables.map(_.applyMatch(m).variables))
              .reduce(_ ++ _)
      if (updatedOtherVariables.termVariables.contains(updatedTermVariable))
        throw DistinctVariableRequirementViolationException(updatedTermVariable)
      updatedTermVariable -> updatedOtherVariables
    })
  }

  def ++(other: DistinctVariableRequirements): DistinctVariableRequirements = {
    DistinctVariableRequirements((map.keySet ++ other.map.keySet).map { variable =>
      variable -> (map.getOrElse(variable, Variables.empty) ++ other.map.getOrElse(variable, Variables.empty))
    }.toMap)
  }

  def filter(f: TermVariable => Boolean): DistinctVariableRequirements = {
    DistinctVariableRequirements(map.filterKeys(f))
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

object DistinctVariableRequirements {
  val empty = DistinctVariableRequirements(Map.empty)
}
