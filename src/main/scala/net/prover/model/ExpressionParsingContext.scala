package net.prover.model

import net.prover.model.ExpressionParsingContext.TermVariableValidator
import net.prover.model.expressions._
import net.prover.model.proof.{StepContext, StepProvingContext}

case class ExpressionParsingContext(
    entryContext: EntryContext,
    termVariableValidator: TermVariableValidator,
    parameterLists: Seq[Seq[(String, Int)]])
  extends ParsingContextWithParameters[ExpressionParsingContext]
{
  def addInitialParameters(numberOfParametersToAdd: Int): ExpressionParsingContext = {
    if (numberOfParametersToAdd > 0)
      copy(parameterLists = (0 until numberOfParametersToAdd).map(i => "$_" + i -> i) +: parameterLists)
    else
      this
  }
  def addInitialParameter(parameter: String): ExpressionParsingContext = {
    copy(parameterLists = Seq((parameter, 0)) +: parameterLists)
  }
  override def addInnerParameters(parameters: Seq[(String, Int)]): ExpressionParsingContext = {
    copy(parameterLists = parameterLists :+ parameters)
  }

  def withPlaceholderParameters(numberOfParameters: Int): ExpressionParsingContext = {
    val parameters = if(numberOfParameters == 1) Seq("$") else (1 to numberOfParameters).map(i => s"$$_$i")
    copy(parameterLists = parameters.zipWithIndex +: parameterLists)
  }

  object RecognisedTermVariableName {
    def unapply(text: String): Option[String] = termVariableValidator.getVariable(text)
  }
  object RecognisedTermVariableOrParameter {
    def unapply(string: String): Option[Term] = {
      termVariableValidator.getVariableOrParameter(string, RecognisedParameter.unapply(string))
    }
  }
}

object ExpressionParsingContext {
  private val StatementVariablePattern: String = "[α-ω]"
  private val TermVariableNamePattern: String = "[^α-ω(){}\\[\\]]"

  object RecognisedStatementVariableName {
    def unapply(text: String): Option[String] = s"($StatementVariablePattern)".r.unapplySeq(text).flatMap(_.headOption)
  }
  object RecognisedDefaultTermVariableName {
    def unapply(text: String): Option[String] = s"($TermVariableNamePattern)".r.unapplySeq(text).flatMap(_.headOption)
  }

  trait TermVariableValidator {
    protected def isValidTermVariable(text: String): Boolean
    def getVariable(text: String): Option[String] = Some(text).filter(isValidTermVariable)
    def getVariableOrParameter(text: String, parameter: Option[FunctionParameter]): Option[Term]
  }
  object TermVariableValidator{
    case object AnyTermVariable extends TermVariableValidator {
      // Matches an optional prime or sub/superscripts
      private val allowedSuffixMatch = "(?:'|[_\\^].*)?"
      private val pattern = s"($TermVariableNamePattern$allowedSuffixMatch)".r
      override protected def isValidTermVariable(text: String): Boolean = pattern.pattern.matcher(text).matches()

      override def getVariableOrParameter(text: String, parameter: Option[FunctionParameter]): Option[Term] = {
        parameter orElse getVariable(text).map(TermVariable(_, Nil))
      }
    }
    case class LimitedList(allowedTermVariables: Seq[String]) extends TermVariableValidator {
      override def isValidTermVariable(text: String): Boolean = allowedTermVariables.contains(text)
      override def getVariableOrParameter(text: String, parameter: Option[FunctionParameter]): Option[Term] = {
        getVariable(text).map(TermVariable(_, Nil)) orElse parameter
      }
    }
  }

  def outsideProof(entryContext: EntryContext): ExpressionParsingContext =
    ExpressionParsingContext(
      entryContext,
      TermVariableValidator.AnyTermVariable,
      Nil)
  def outsideProof(entryContext: EntryContext, termVariableNames: Seq[String]): ExpressionParsingContext =
    ExpressionParsingContext(
      entryContext,
      TermVariableValidator.LimitedList(termVariableNames),
      Nil)
  implicit def atStep(implicit entryContext: EntryContext, stepContext: StepContext): ExpressionParsingContext = {
    ExpressionParsingContext(
      entryContext,
      TermVariableValidator.LimitedList(stepContext.termVariableNames),
      stepContext.boundVariableLists.map(_.zipWithIndex))
  }
  implicit def atStep(stepProvingContext: StepProvingContext): ExpressionParsingContext = {
    atStep(stepProvingContext.provingContext.entryContext, stepProvingContext.stepContext)
  }
}
