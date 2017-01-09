package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

case class DistinctVariableViolationException(
    variable: TermVariable)
  extends Exception(
    s"Distinct variable violated: $variable")

case class DistinctVariables(map: Map[TermVariable, Variables]) extends JsonSerializable.Base {
  def applySubstitutions(
    substitutions: Substitutions
  ): DistinctVariables = {
    DistinctVariables(map.map { case (termVariable, Variables(statementVariables, termVariables)) =>
      val updatedTermVariable = Term.asVariable(termVariable.applySubstitutions(substitutions))
      val updatedStatementVariables = statementVariables.map(
        _.applySubstitutions(substitutions).variables)
      val updatedTermVariables = termVariables.map(
        _.applySubstitutions(substitutions).variables)
      val updatedOtherVariables = (updatedStatementVariables ++ updatedTermVariables).reduce(_ ++ _)
      if (updatedOtherVariables.termVariables.contains(updatedTermVariable))
        throw DistinctVariableViolationException(updatedTermVariable)
      updatedTermVariable -> updatedOtherVariables
    })
  }

  def +(tuple: (TermVariable, StatementVariable)): DistinctVariables = {
    val variables = map.getOrElse(tuple._1, Variables.empty)
    val updatedMap = map + (tuple._1 -> (variables :+ tuple._2))
    copy(map = updatedMap)
  }

  def ++(other: DistinctVariables): DistinctVariables = {
    DistinctVariables((map.keySet ++ other.map.keySet).map { variable =>
      variable -> (map.getOrElse(variable, Variables.empty) ++ other.map.getOrElse(variable, Variables.empty))
    }.toMap)
  }

  def filter(f: StatementVariable => Boolean, g: TermVariable => Boolean): DistinctVariables = {
    DistinctVariables(map.filterKeys(g).mapValues(x => x.filter(f, g)).filter {
      case (_, variables) => variables.nonEmpty
    })
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
  val empty = DistinctVariables(Map.empty)

  def parse(line: PartialLine, context: Context): (DistinctVariables, PartialLine) = {
    def parseIndividualRequirements(
      remainingLine: PartialLine,
      distinctVariables: DistinctVariables = DistinctVariables.empty
    ): (DistinctVariables, PartialLine) = {
      if (remainingLine.remainingText.head == ')') {
        (distinctVariables, remainingLine)
      } else {
        val (term, lineAfterTerm) =
          Term.parse(remainingLine, context)
            .mapLeft(Term.asVariable)
        val (statement, lineAfterStatement) =
          Statement.parseStatementVariable(lineAfterTerm, context)
        val updatedDistinctVariables = distinctVariables + (term -> statement)
        lineAfterStatement match {
          case WordAndRemainingText(",", lineAfterComma) =>
            parseIndividualRequirements(lineAfterComma, updatedDistinctVariables)
          case _ =>
            (updatedDistinctVariables, lineAfterStatement)
        }
      }
    }
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    val (distinctVariables, lineAfterRequirements) = parseIndividualRequirements(line.tail)
    if (lineAfterRequirements.remainingText.head != ')') {
      throw ParseException.withMessage("Close-paren expected but not found", line.fullLine)
    }
    (distinctVariables, lineAfterRequirements.tail)
  }
}
