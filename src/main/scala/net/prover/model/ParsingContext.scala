package net.prover.model

import net.prover.model.entries.{ChapterEntry, StatementDefinition, TermDefinition}

case class ParsingContext(
    statementDefinitions: Seq[StatementDefinition],
    termDefinitions: Seq[TermDefinition],
    statementVariableNames: Set[String],
    termVariableNames: Set[String]) {

  def add(chapterEntry: ChapterEntry): ParsingContext = chapterEntry match {
    case statementDefinition: StatementDefinition =>
      copy(statementDefinitions = statementDefinitions :+ statementDefinition)
    case termDefinition: TermDefinition =>
      copy(termDefinitions = termDefinitions :+ termDefinition)
    case _ =>
      this
  }

  def combine(others: Seq[ParsingContext]): ParsingContext = {
    ParsingContext(
      others.flatMap(_.statementDefinitions) ++ statementDefinitions,
      others.flatMap(_.termDefinitions) ++ termDefinitions,
      statementVariableNames,
      termVariableNames)
  }

  def addStatementDefinition(statementDefinition: StatementDefinition): ParsingContext = {
    copy(statementDefinitions = statementDefinitions :+ statementDefinition)
  }

  def addTermDefinition(termDefinition: TermDefinition) = {
    copy(termDefinitions = termDefinitions :+ termDefinition)
  }
}

object ParsingContext {
  val empty = ParsingContext(
    statementDefinitions = Nil,
    termDefinitions = Nil,
    statementVariableNames = Set.empty,
    termVariableNames = Set.empty)

  def combine(contexts: Seq[ParsingContext]): ParsingContext = {
    ParsingContext(
      contexts.flatMap(_.statementDefinitions),
      contexts.flatMap(_.termDefinitions),
      Set.empty,
      Set.empty)
  }
}
