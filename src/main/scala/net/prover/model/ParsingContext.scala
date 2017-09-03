package net.prover.model

import net.prover.model.components.{BoundVariable, StatementVariable, TermVariable}
import net.prover.model.entries.{ChapterEntry, StatementDefinition, TermDefinition}

case class ParsingContext(
    statementDefinitions: Seq[StatementDefinition],
    termDefinitions: Seq[TermDefinition],
    statementVariableNames: Set[String],
    termVariableNames: Set[String],
    boundVariableNames: Seq[String]) {

  def add(chapterEntry: ChapterEntry): ParsingContext = chapterEntry match {
    case statementDefinition: StatementDefinition =>
      copy(statementDefinitions = statementDefinitions :+ statementDefinition)
    case termDefinition: TermDefinition =>
      copy(termDefinitions = termDefinitions :+ termDefinition)
    case _ =>
      this
  }

  def addStatementDefinition(statementDefinition: StatementDefinition): ParsingContext = {
    copy(statementDefinitions = statementDefinitions :+ statementDefinition)
  }

  def addTermDefinition(termDefinition: TermDefinition) = {
    copy(termDefinitions = termDefinitions :+ termDefinition)
  }

  def addBoundVariable(variableName: String): ParsingContext = {
    copy(boundVariableNames = boundVariableNames :+ variableName)
  }

  object RecognisedStatementVariable {
    def unapply(string: String): Option[StatementVariable] = {
      if (statementVariableNames.contains(string)) {
        Some(StatementVariable(string))
      } else {
        None
      }
    }
  }
  object RecognisedStatementDefinition {
    def unapply(string: String): Option[StatementDefinition] = {
      statementDefinitions.find(_.symbol == string)
    }
  }

  object RecognisedTermVariable {
    def unapply(string: String): Option[TermVariable] = {
      def existsDirectly: Boolean = termVariableNames.contains(string)
      def existsWithPrime: Boolean = termVariableNames.exists(_ + "'" == string)
      def existsWithSubscript: Boolean = {
        val index = string.indexOf('_')
        (index >= 0) && termVariableNames.contains(string.substring(0, index))
      }
      if (existsDirectly || existsWithPrime || existsWithSubscript) {
        Some(TermVariable(string))
      } else {
        None
      }
    }
  }
  object RecognisedBoundVariable {
    def unapply(string: String): Option[BoundVariable] = {
      boundVariableNames.zipWithIndex.find(_._1 == string).map(_._2).map(BoundVariable.apply)
    }
  }
}

object ParsingContext {
  val empty = ParsingContext(
    statementDefinitions = Nil,
    termDefinitions = Nil,
    statementVariableNames = Set.empty,
    termVariableNames = Set.empty,
    boundVariableNames = Seq.empty)
}
