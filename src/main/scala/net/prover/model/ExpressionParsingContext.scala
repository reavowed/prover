package net.prover.model

import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.expressions.{StatementVariable, Term, TermVariable}
import net.prover.model.proof.{StepContext, StepProvingContext}

case class ExpressionParsingContext(
    entryContext: EntryContext,
    variableDefinitions: VariableDefinitions,
    parameterLists: Seq[Seq[(String, Int)]])
  extends ParsingContextWithParameters[ExpressionParsingContext]
{
  def addInitialParameters(numberOfParametersToAdd: Int): ExpressionParsingContext = {
    if (numberOfParametersToAdd > 0)
      copy(parameterLists = (0 until numberOfParametersToAdd).map(i => "$_" + i -> i) +: parameterLists)
    else
      this
  }
  def addInitialParameters(parameterNames: Seq[String]): ExpressionParsingContext = {
    if (parameterNames.nonEmpty)
      copy(parameterLists = parameterNames.zipWithIndex +: parameterLists)
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

  def addSimpleTermVariables(additionalTermVariableNames: Seq[String]): ExpressionParsingContext = {
    copy(variableDefinitions = variableDefinitions.addSimpleTermVariables(additionalTermVariableNames))
  }

  private def getVariable[T](name: String, arguments: Seq[Term], recogniser: String => Option[String], description: String, constructor: (String, Seq[Term]) => T): Option[T] = {
    recogniser(name).map { name =>
      constructor(name, arguments)
    }
  }

  def getStatementVariable(name: String, arguments: Seq[Term]): Option[StatementVariable] = {
    getVariable(name, arguments, ExpressionParsingContext.RecognisedStatementVariableName.unapply, "statement", StatementVariable.apply)
  }
  def getTermVariable(name: String, arguments: Seq[Term]): Option[TermVariable] = {
    getVariable(name, arguments, ExpressionParsingContext.RecognisedTermVariableName.unapply, "term", TermVariable.apply)
  }

  object SimpleStatementVariable {
    def unapply(name: String): Option[StatementVariable] = {
      getStatementVariable(name, Nil)
    }
  }
  object SimpleTermVariable {
    def unapply(name: String): Option[TermVariable] = {
      getTermVariable(name, Nil)
    }
  }
}

object ExpressionParsingContext {
  private val StatementVariablePattern: String = "[α-ω]"
  private val TermVariableNamePattern: String = "[^α-ω(){}\\[\\]]"
  // Matches an optional prime or sub/superscripts
  private val TermVariableSuffixPattern = "(?:'|[_\\^].*)?"

  object RecognisedStatementVariableName {
    def unapply(text: String): Option[String] = s"($StatementVariablePattern)".r.unapplySeq(text).flatMap(_.headOption)
  }
  object RecognisedTermVariableName {
    def unapply(text: String): Option[String] = s"($TermVariableNamePattern$TermVariableSuffixPattern)".r.unapplySeq(text).flatMap(_.headOption)
  }

  def forInference(inference: Inference)(implicit entryContext: EntryContext): ExpressionParsingContext = withDefinitions(inference.variableDefinitions)
  def forComponentTypes(componentTypes: Seq[ComponentType])(implicit entryContext: EntryContext): ExpressionParsingContext = {
    withDefinitions(VariableDefinitions.fromComponentTypes(componentTypes))
  }
  def forTypeDefinition(termNames: Seq[String])(implicit entryContext: EntryContext): ExpressionParsingContext = {
    withDefinitions(VariableDefinitions.empty.addSimpleTermVariables(termNames))
  }

  def withDefinitions(variableDefinitions: VariableDefinitions)(implicit entryContext: EntryContext): ExpressionParsingContext = {
    ExpressionParsingContext(entryContext, variableDefinitions, Nil)
  }

  implicit def atStep(implicit entryContext: EntryContext, stepContext: StepContext): ExpressionParsingContext = {
    ExpressionParsingContext(
      entryContext,
      stepContext.variableDefinitions,
      stepContext.boundVariableLists.map(_.zipWithIndex))
  }
  implicit def atStep(stepProvingContext: StepProvingContext): ExpressionParsingContext = {
    atStep(stepProvingContext.provingContext.entryContext, stepProvingContext.stepContext)
  }
}
