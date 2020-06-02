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
    copy(variableDefinitions = variableDefinitions.addSimpleTermVariableNames(additionalTermVariableNames))
  }

  private def getVariable[T](name: String, arguments: Seq[Term], definitions: Seq[VariableDefinition], prefix: String, description: String, constructor: (Int, Seq[Term]) => T): Option[T] = {
    val definition = definitions.zipWithIndex.find { case (definition, _) => definition.name == name } orElse
      s"$prefix(\\d+)".r.unapplySeq(name).flatMap(_.single).map(_.toInt).flatMap(i => definitions.lift(i).map(_ -> i))

    definition.map { case (definition, index) =>
      if (definition.arity != arguments.length) throw new Exception(s"${description.capitalize} variable $name requires ${definition.arity} parameters")
      constructor(index, arguments)
    }
  }

  def getStatementVariable(name: String, arguments: Seq[Term]): Option[StatementVariable] = {
    getVariable(name, arguments, variableDefinitions.statements, "s", "statement", StatementVariable.apply)
  }
  def getTermVariable(name: String, arguments: Seq[Term]): Option[TermVariable] = {
    getVariable(name, arguments, variableDefinitions.terms, "t", "term", TermVariable.apply)
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
  def forTypeDefinition(termVariableDefinitions: Seq[SimpleVariableDefinition])(implicit entryContext: EntryContext): ExpressionParsingContext = {
    withDefinitions(VariableDefinitions.empty.addSimpleTermVariables(termVariableDefinitions))
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
