package net.prover.model

import java.util.regex.Pattern

import net.prover.model.entries.{StatementDefinition, TermDefinition}
import net.prover.model.expressions._
import net.prover.model.proof.StepContext

import scala.util.Try
import scala.util.matching.Regex

case class ExpressionParsingContext(
    entryContext: EntryContext,
    parameterLists: Seq[Seq[(String, Int)]])
{
  def parameterDepth: Int = parameterLists.length

  def addParameters(parameters: String *): ExpressionParsingContext = {
    copy(parameterLists = parameterLists :+ parameters.zipWithIndex)
  }

  def addParameterList(parameters: Seq[(String, Int)]): ExpressionParsingContext = {
    copy(parameterLists = parameterLists :+ parameters)
  }

  def withPlaceholderParameters(numberOfParameters: Int): ExpressionParsingContext = {
    val parameters = if(numberOfParameters == 1) Seq("$") else (1 to numberOfParameters).map(i => s"$$_$i")
    copy(parameterLists = parameters.zipWithIndex +: parameterLists)
  }

  object RecognisedStatementVariable {
    def unapply(string: String): Option[String] = {
      "([α-ω])".r.unapplySeq(string).flatMap(_.headOption)
    }
  }
  object RecognisedStatementDefinition {
    def unapply(string: String): Option[StatementDefinition] = {
      entryContext.statementDefinitions.find(_.symbol == string)
    }
  }

  object RecognisedTermVariable {
    def unapply(string: String): Option[String] = {
      val allowedVariableMatches = ("[a-zA-Z]" +: entryContext.termVariableNames.map(Pattern.quote)).mkString("|")
      // Matches an optional prime or sub/superscripts
      val allowedSuffixMatch = "(?:'|[_\\^].*)?"
      val pattern = s"($allowedVariableMatches$allowedSuffixMatch)".r

      pattern.unapplySeq(string).flatMap(_.headOption)
    }
  }
  object RecognisedTermDefinition {
    def unapply(s: String): Option[TermDefinition] = {
      entryContext.termDefinitions.find(_.symbol == s)
    }
  }

  object RecognisedParameter {
    private val literalPattern = "(\\$+)(\\d+)".r
    def unapply(string: String): Option[FunctionParameter] = {
      parameterLists.reverse.zipWithIndex.mapFind {
        case (parameterList, level) =>
          parameterList.find(_._1 == string).map(_._2).map(index => FunctionParameter(index, level))
      } orElse (string match {
        case literalPattern(dollars, indexString) =>
          val level = dollars.length - 1
          for {
            index <- Try(indexString.toInt).toOption
          } yield FunctionParameter(index, level)
        case _ =>
          None
      })
    }
  }
}

object ExpressionParsingContext {
  def outsideProof(entryContext: EntryContext): ExpressionParsingContext =
    ExpressionParsingContext(entryContext, Nil)
  implicit def atStep(implicit entryContext: EntryContext, stepContext: StepContext): ExpressionParsingContext =
    ExpressionParsingContext(entryContext, stepContext.boundVariableLists.map(_.zipWithIndex))
}
