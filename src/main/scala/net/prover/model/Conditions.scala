package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

case class Conditions(
    arbitraryVariables: Set[TermVariable],
    distinctVariables: Map[TermVariable, Variables])
  extends JsonSerializable.Base {

  def ++(other: Conditions): Conditions = {
    Conditions(
      arbitraryVariables ++ other.arbitraryVariables,
      (distinctVariables.keySet ++ other.distinctVariables.keySet).map { variable =>
        variable -> (
          distinctVariables.getOrElse(variable, Variables.empty) ++
            other.distinctVariables.getOrElse(variable, Variables.empty))
      }.toMap)
  }

  def addDistinctVariables(newDistinctVariables: Map[TermVariable, Variables]): Conditions = {
    copy(distinctVariables = distinctVariables ++ newDistinctVariables)
  }

  def addDistinctVariables(variables: Set[TermVariable], statements: Seq[Statement]): Option[Conditions] = {
    variables
      .map { termVariable =>
        val variables = statements.map(_.getPotentiallyIntersectingVariables(termVariable)).reduceOption(_ ++ _).getOrElse(Variables.empty)
        if (variables.termVariables.contains(termVariable))
          None
        else
          Some(termVariable -> variables)
      }
      .traverseOption
      .map(_.toMap.filter(_._2.nonEmpty))
      .map(addDistinctVariables)
  }

  def restrictToStatements(statements: Seq[Statement]): Conditions = {
    val activeVariables = statements.map(_.allVariables).reduce(_ ++ _)
    val updatedDistinctVariables = distinctVariables
      .mapValues(_ intersect activeVariables)
      .filter { case (termVariable, variables) =>
        activeVariables.termVariables.contains(termVariable) && variables.nonEmpty
      }
    val updatedArbitraryVariables = arbitraryVariables
      .intersect(activeVariables.termVariables)
      .filter { v =>
        val threats = statements.map(_.getPotentiallyIntersectingVariables(v)).reduce(_ ++ _)
        val protectors = distinctVariables.getOrElse(v, Variables.empty)
        !(threats.statementVariables.forall(w => protectors.statementVariables.contains(w)) &&
          threats.termVariables.forall(w => protectors.termVariables.contains(w) || distinctVariables.getOrElse(w, Variables.empty).termVariables.contains(v)))
      }
    Conditions(updatedArbitraryVariables, updatedDistinctVariables)
  }

  def filterOutBoundVariables(boundVariables: Set[TermVariable]): Conditions = {
    copy(arbitraryVariables = arbitraryVariables.diff(boundVariables))
  }

  def restrictToActiveVariables(activeVariables: Variables): Conditions = {
    copy(
      arbitraryVariables = arbitraryVariables.intersect(activeVariables.termVariables),
      distinctVariables = distinctVariables
        .mapValues(_ intersect activeVariables)
        .filter { case (termVariable, variables) =>
          activeVariables.termVariables.contains(termVariable) && variables.nonEmpty
        })
  }

  def applySubstitutions(
    substitutions: Substitutions
  ): Option[Conditions] = {
    for {
      updatedArbitraryVariables <- arbitraryVariables
        .map(_.applySubstitutions(substitutions).flatMap(Term.optionAsVariable))
        .traverseOption
      updatedDistinctVariables <- distinctVariables
        .map { case (termVariable, variables) =>
            substituteDistinctVariableCondition(termVariable, variables, substitutions)
        }
        .traverseOption.map(_.toMap)
    } yield {
      Conditions(updatedArbitraryVariables, updatedDistinctVariables)
    }
  }

  private def substituteDistinctVariableCondition(
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
  val empty = Conditions(Set.empty, Map.empty)

  def arbitraryVariablesParser(implicit context: Context): Parser[Option[Set[TermVariable]]] = {
    Parser.optional(
      "arbitrary-variables",
      Term.variableListParser.map(_.toSet).map(Some.apply),
      None)
  }

  private def variableParser(variables: Variables)(implicit context: Context): Parser[Option[Variables]] = {
    Parser.singleWord.map { name =>
      val statementVariableOption = context.variables.statementVariables.find(_.text == name)
      val termVariableOption = context.variables.termVariables.find(_.text == name)
      statementVariableOption.map(variables + _) orElse
        termVariableOption.map(variables + _)
    }
  }

  private def distinctVariablesClauseParser(implicit context: Context): Parser[(TermVariable, Variables)] = {
    for {
      term <- Term.variableParser
      variables <- Parser.iterateWhileDefined(Variables.empty, variableParser)
    } yield term -> variables
  }

  def optionalDistinctVariablesParser(implicit context: Context): Parser[Option[Map[TermVariable, Variables]]] = {
    Parser.optional(
      "distinct-variables",
      distinctVariablesClauseParser.listInParens(Some(",")).map(_.toMap).map(Some.apply),
      None)
  }

  def distinctVariablesParser(implicit context: Context): Parser[Map[TermVariable, Variables]] = {
    optionalDistinctVariablesParser.getOrElse(Map.empty)
  }

  def optionalParser(implicit context: Context): Parser[Option[Conditions]] = {
    for {
      arbitraryVariablesOption <- arbitraryVariablesParser
      distinctVariablesOption <- optionalDistinctVariablesParser
    } yield {
      if (arbitraryVariablesOption.isEmpty && distinctVariablesOption.isEmpty)
        None
      else
        Some(Conditions(
          arbitraryVariablesOption.getOrElse(Set.empty),
          distinctVariablesOption.getOrElse(Map.empty)))
    }
  }

  def parser(implicit context: Context): Parser[Conditions] = {
    optionalParser.getOrElse(Conditions.empty)
  }
}
