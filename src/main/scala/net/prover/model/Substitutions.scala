package net.prover.model

import net.prover.model.components._

case class Substitutions(componentsByVariable: Map[Variable, Component])
{
  def tryAdd(variable: Variable, component: Component): Option[Substitutions] = {
    componentsByVariable.get(variable) match {
      case Some(`component`) =>
        Some(this)
      case Some(_) =>
        None
      case None =>
        Some(copy(componentsByVariable = componentsByVariable.updated(variable, component)))
    }
  }

  def serialized: String = {
    val serializedComponentsByVariable = componentsByVariable.toSeq
      .sortBy { case (variable, _) => variable.text }
      .map { case (variable, component) => s"${variable.serialized} -> ${component.serialized}"}
      .mkString(", ")
    s"($serializedComponentsByVariable)"
  }
}

object Substitutions {
  val empty = Substitutions(Map.empty)
  def parser(implicit parsingContext: ParsingContext): Parser[Substitutions] = {
    for {
      componentsByVariable <- pairParser.listInParens(Some(",")).map(_.toMap)
    } yield {
      Substitutions(componentsByVariable)
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
