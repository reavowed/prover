package net.prover.model

import net.prover.model.expressions._
import net.prover.model.entries.{ChapterEntry, StatementDefinition, TermDefinition}

import scala.util.Try

case class ParsingContext(
    statementDefinitions: Seq[StatementDefinition],
    termDefinitions: Seq[TermDefinition],
    statementVariableNames: Set[String],
    termVariableNames: Set[String],
    parameterLists: Seq[Seq[String]])
{
  def parameterDepth: Int = parameterLists.length

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

  def addParameterList(parameterList: Seq[String]) = {
    copy(parameterLists = parameterLists :+ parameterList)
  }

  object RecognisedStatementVariable {
    def unapply(string: String): Option[String] = {
      Some(string).filter(statementVariableNames.contains)
    }
  }
  object RecognisedStatementDefinition {
    def unapply(string: String): Option[StatementDefinition] = {
      statementDefinitions.find(_.symbol == string)
    }
  }

  object RecognisedTermVariable {
    def unapply(string: String): Option[String] = {
      def existsDirectly: Boolean = termVariableNames.contains(string)
      def existsWithPrime: Boolean = termVariableNames.exists(_ + "'" == string)
      def existsWithSubscript: Boolean = {
        val index = string.indexOf('_')
        (index >= 0) && termVariableNames.contains(string.substring(0, index))
      }
      if (existsDirectly || existsWithPrime || existsWithSubscript) {
        Some(string)
      } else {
        None
      }
    }
  }
  object RecognisedTermDefinition {
    def unapply(s: String): Option[TermDefinition] = {
      termDefinitions.find(_.symbol == s)
    }
  }

  object RecognisedParameter {
    val literalPattern = "(\\$+)(\\.*)(.*)".r
    def unapply(string: String): Option[FunctionParameter] = {
      parameterLists.zipWithIndex.mapFind {
        case (parameterList, depth) =>
          parameterList.findIndexWhere(_ == string).map(index => FunctionParameter(index, depth + 1, parameterDepth)(Some(string)))
      } orElse (string match {
        case literalPattern(dollars, dots, indexString) =>
          val level = dollars.length
          val depth = level + dots.length
          for {
            index <- Try(indexString.toInt).toOption
            name = parameterLists.lift(level).flatMap(_.lift(index)).getOrElse(dollars + index)
          } yield FunctionParameter(index, level, depth)(None)
        case _ =>
          None
      })
    }
  }
}

object ParsingContext {
  val empty = ParsingContext(
    statementDefinitions = Nil,
    termDefinitions = Nil,
    statementVariableNames = Set.empty,
    termVariableNames = Set.empty,
    parameterLists = Seq.empty)
}
