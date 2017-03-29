package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

case class Conditions(
    arbitraryVariables: Seq[TermVariable],
    distinctVariables: Map[TermVariable, Variables])
  extends JsonSerializable.Base {

  def ++(other: Conditions): Conditions = {
    Conditions(
      (arbitraryVariables ++ other.arbitraryVariables).distinct,
      (distinctVariables.keySet ++ other.distinctVariables.keySet).map { variable =>
        variable -> (
          distinctVariables.getOrElse(variable, Variables.empty) ++
            other.distinctVariables.getOrElse(variable, Variables.empty))
      }.toMap)
  }

  def restrictTo(termVariables: Seq[TermVariable]): Conditions = {
    Conditions(
      arbitraryVariables.intersect(termVariables),
      distinctVariables.filterKeys(termVariables.contains))
  }

  def applySubstitutions(
    substitutions: Substitutions
  ): Conditions = Conditions(
    arbitraryVariables
      .map(_.applySubstitutions(substitutions))
      .map(Term.asVariable),
    distinctVariables.map { case (termVariable, Variables(statementVariables, termVariables)) =>
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

  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartObject()
    gen.writeObjectField("arbitraryVariables", arbitraryVariables)
    gen.writeFieldName("distinctVariables")
    gen.writeStartArray()
    distinctVariables.foreach { case (termVariable, variables) =>
      gen.writeStartObject()
      gen.writeObjectField("termVariable", termVariable)
      gen.writeObjectField("variables", variables.statementVariables ++ variables.termVariables)
      gen.writeEndObject()
    }
    gen.writeEndArray()
    gen.writeEndObject()
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

object Conditions {
  val empty = Conditions(Seq.empty, Map.empty)

  def arbitraryVariablesParser(implicit context: Context): Parser[Seq[TermVariable]] = {
    Parser.optional("arbitrary-variables", Term.variableListParser, Nil)
  }

  private def variableParser(variables: Variables)(implicit context: Context): Parser[Option[Variables]] = {
    Parser.singleWord.map { name =>
      val statementVariableOption = context.variables.statementVariables.find(_.text == name)
      val termVariableOption = context.variables.termVariables.find(_.text == name)
      statementVariableOption.map(variables :+ _) orElse
        termVariableOption.map(variables :+ _)
    }
  }

  private def distinctVariablesClauseParser(implicit context: Context): Parser[(TermVariable, Variables)] = {
    for {
      term <- Term.variableParser
      variables <- Parser.iterateWhileDefined(Variables.empty, variableParser)
    } yield term -> variables
  }

  def distinctVariablesParser(implicit context: Context): Parser[Map[TermVariable, Variables]] = {
    Parser.optional(
      "distinct-variables",
      distinctVariablesClauseParser.listInParens(Some(",")).map(_.toMap),
      Map.empty)
  }

  def parser(implicit context: Context): Parser[Conditions] = {
    for {
      arbitraryVariables <- arbitraryVariablesParser
      distinctVariables <- distinctVariablesParser
    } yield Conditions(arbitraryVariables, distinctVariables)
  }
}
