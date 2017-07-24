package net.prover.model

import net.prover.model.components._

case class Substitutions(
    componentsByVariable: Map[Variable, Component],
    distinctVariables: DistinctVariables)
{
  def serialized: String = {
    val serializedComponentsByVariable = componentsByVariable.toSeq
      .sortBy { case (variable, component) => variable.text }
      .map { case (variable, component) => s"${variable.serialized} -> ${component.serialized}"}
      .mkString(", ")
    val serializedDistinctVariables = distinctVariables.serialized
    s"($serializedComponentsByVariable) ($serializedDistinctVariables)"
  }

  def toPartial: PartialSubstitutions = PartialSubstitutions(componentsByVariable, Map.empty, distinctVariables)
}

object Substitutions {
  def parser(implicit parsingContext: ParsingContext): Parser[Substitutions] = {
    for {
      componentsByVariable <- pairParser.listInParens(Some(",")).map(_.toMap)
      distinctVariables <- DistinctVariables.parser
    } yield {
      Substitutions(componentsByVariable, distinctVariables)
    }
  }
  private def pairParser(implicit parsingContext: ParsingContext): Parser[(Variable, Component)] = {
    for {
      variable <- Variable.parser
      _ <- Parser.requiredWord("->")
      component <- Component.parser
    } yield (variable, component)
  }
}
