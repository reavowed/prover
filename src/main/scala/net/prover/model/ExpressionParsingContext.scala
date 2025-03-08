package net.prover.model

import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.expressions.{ExpressionVariable, Term}
import net.prover.model.proof.{StepContext, StepProvingContext}
import net.prover.parsing.{KnownWordParser, ParseException, Parser}

case class ExpressionParsingContext(
    availableEntries: AvailableEntries,
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

  private def getVariableDefinition(
    identifier: String,
    definitions: Seq[VariableDefinition],
    prefix: String
  ): Option[(VariableDefinition, Int)] = {
    definitions.zipWithIndex.find { case (definition, _) => definition.name == identifier } orElse
      s"$prefix(\\d+)".r.unapplySeq(identifier).flatMap(_.single).map(_.toInt).flatMap(i => definitions.lift(i).map(_ -> i))
  }

  private def variableDefinitionParser(
    definitions: Seq[VariableDefinition],
    prefix: String,
    description: String
  ): KnownWordParser[(VariableDefinition, Int)] = {
    KnownWordParser(
      getVariableDefinition(_, definitions, prefix).map(Parser.constant),
      description)
  }
  def statementVariableDefinitionParser: KnownWordParser[(VariableDefinition, Int)] = {
    variableDefinitionParser(variableDefinitions.statements, "s", "statement variable name")
  }
  def termVariableDefinitionParser: KnownWordParser[(VariableDefinition, Int)] = {
    variableDefinitionParser(variableDefinitions.terms, "t", "term variable name")
  }

  def simpleVariableParser[T <: ExpressionVariable[_]](
    definitionParser: KnownWordParser[(VariableDefinition, Int)],
    constructor: (Int, Seq[Term]) => T,
    description: String
  ): KnownWordParser[T] = {
    for {
      definitionAndIndex <- definitionParser
      (definition, index) = definitionAndIndex
      _ = if (definition.arity != 0) throw ParseException(s"Missing arguments for $description variable ${definition.name}")
    } yield constructor(index, Nil)
  }
  def variableApplicationParser[T <: ExpressionVariable[_]](
    definitionParser: KnownWordParser[(VariableDefinition, Int)],
    constructor: (Int, Seq[Term]) => T,
    description: String
  ): KnownWordParser[T] = {
    KnownWordParser("with") {
      for {
        arguments <- Term.parser(this).listInParensOrSingle(None)
        definitionAndIndex <- definitionParser
        (definition, index) = definitionAndIndex
        _ = if (arguments.length != definition.arity)
          throw ParseException(s"Invalid number of arguments for ${description} variable ${definition.name} - " +
            "expected " + arguments.length + ", got " + arguments.length)
      } yield constructor(index, arguments)
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

  def forInference(inference: Inference)(implicit availableEntries: AvailableEntries): ExpressionParsingContext = withDefinitions(inference.variableDefinitions)
  def forComponentTypes(componentTypes: Seq[ComponentType])(implicit availableEntries: AvailableEntries): ExpressionParsingContext = {
    withDefinitions(VariableDefinitions.fromComponentTypes(componentTypes))
  }
  def forTypeDefinition(termVariableDefinitions: Seq[SimpleVariableDefinition])(implicit availableEntries: AvailableEntries): ExpressionParsingContext = {
    withDefinitions(VariableDefinitions.empty.addSimpleTermVariables(termVariableDefinitions))
  }

  def withDefinitions(variableDefinitions: VariableDefinitions)(implicit availableEntries: AvailableEntries): ExpressionParsingContext = {
    ExpressionParsingContext(availableEntries, variableDefinitions, Nil)
  }

  implicit def atStep(implicit stepContext: StepContext, provingContext: ProvingContext): ExpressionParsingContext = {
    ExpressionParsingContext(
      provingContext.availableEntries,
      stepContext.variableDefinitions,
      stepContext.boundVariableLists.map(_.zipWithIndex))
  }
  def atStep(stepProvingContext: StepProvingContext): ExpressionParsingContext = {
    atStep(stepProvingContext.stepContext, stepProvingContext.provingContext)
  }
}
