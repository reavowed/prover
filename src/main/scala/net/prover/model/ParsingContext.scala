package net.prover.model

import net.prover.model.entries.{ChapterEntry, StatementDefinition, TermDefinition}
import net.prover.model.expressions._
import net.prover.model.proof.Transformation

import scala.util.Try

case class ParsingContext(
    inferences: Seq[Inference],
    statementDefinitions: Seq[StatementDefinition],
    termDefinitions: Seq[TermDefinition],
    termVariableNames: Set[String],
    parameterLists: Seq[Seq[(String, Int)]])
{
  def parameterDepth = parameterLists.length
  def deductionStatement = statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Deduction))
  def scopingStatement = statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Scoping))
  def transformation: Option[Transformation] = scopingStatement.flatMap(Transformation.find(_, inferences))

  def add(chapterEntry: ChapterEntry): ParsingContext = {
    val contextWithDefinitions = chapterEntry match {
      case statementDefinition: StatementDefinition =>
        copy(statementDefinitions = statementDefinitions :+ statementDefinition)
      case termDefinition: TermDefinition =>
        copy(termDefinitions = termDefinitions :+ termDefinition)
      case _ =>
        this
    }
    contextWithDefinitions.copy(inferences = inferences ++ chapterEntry.inferences)
  }

  def addStatementDefinition(statementDefinition: StatementDefinition): ParsingContext = {
    copy(statementDefinitions = statementDefinitions :+ statementDefinition)
  }

  def addTermDefinition(termDefinition: TermDefinition) = {
    copy(termDefinitions = termDefinitions :+ termDefinition)
  }

  def addParameters(parameters: String *) = {
    copy(parameterLists = parameterLists :+ parameters.zipWithIndex)
  }

  def addParameterList(parameters: Seq[(String, Int)]) = {
    copy(parameterLists = parameterLists :+ parameters)
  }

  object RecognisedStatementVariable {
    def unapply(string: String): Option[String] = {
      "([α-ω])".r.unapplySeq(string).flatMap(_.headOption)
    }
  }
  object RecognisedStatementDefinition {
    def unapply(string: String): Option[StatementDefinition] = {
      statementDefinitions.find(_.symbol == string)
    }
  }

  object RecognisedTermVariable {
    def unapply(string: String): Option[String] = {
      "([a-zA-Z](?:'|_\\w+)?)".r.unapplySeq(string).flatMap(_.headOption) orElse
        termVariableNames.find(_ == string)
    }
  }
  object RecognisedTermDefinition {
    def unapply(s: String): Option[TermDefinition] = {
      termDefinitions.find(_.symbol == s)
    }
  }

  object RecognisedParameter {
    val literalPattern = "(\\$+)(.*)".r
    def unapply(string: String): Option[FunctionParameter] = {
      parameterLists.reverse.zipWithIndex.mapFind {
        case (parameterList, level) =>
          parameterList.find(_._1 == string).map(_._2).map(index => FunctionParameter(index, level)(Some(string)))
      } orElse (string match {
        case literalPattern(dollars, indexString) =>
          val level = dollars.length - 1
          for {
            index <- Try(indexString.toInt).toOption
            name = parameterLists.reverse.lift(level).flatMap(_.lift(index)).map(_._1)
          } yield FunctionParameter(index, level)(name)
        case _ =>
          None
      })
    }
  }
}

object ParsingContext {
  val empty = ParsingContext(
    inferences = Nil,
    statementDefinitions = Nil,
    termDefinitions = Nil,
    termVariableNames = Set.empty,
    parameterLists = Seq.empty)
}
