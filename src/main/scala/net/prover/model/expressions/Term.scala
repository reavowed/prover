package net.prover.model.expressions

import net.prover.core.transformers.{ContextWithExternalDepth, ContextWithInternalDepth}
import net.prover.model.{ExpressionParsingContext, Parser, Substitutions, TemplateParsingContext}
import net.prover.old.OldParameterInserter
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.substitutionFinding.transformers.{PossibleSubstitutionCalculationParameters, PossibleSubstitutionCalculator}

trait Term extends Expression with TypedExpression[Term] {
  override def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, Term, Int, Seq[Int])] = Seq((this, FunctionParameter(0, internalDepth + externalDepth), internalDepth, Nil))
  def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: PossibleSubstitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(Term, PossibleSubstitutions)] = {
    for {
      (argument, index) <- baseArguments.zipWithIndex.iterator
      updatedSubstitutions <- PossibleSubstitutionCalculator.calculateFromExpressionWithContext(
        OldParameterInserter.insertParameters(argument, internalDepth, 0),
        this,
        PossibleSubstitutionCalculationParameters(substitutions, ContextWithExternalDepth(externalDepth)))(
        ContextWithInternalDepth(previousInternalDepth + internalDepth))
    } yield FunctionParameter(index, externalDepth + internalDepth) -> updatedSubstitutions
  }
}

object Term {
  def asVariable(expression: Expression): TermVariable = {
    optionAsVariable(expression).getOrElse(throw new Exception(s"Expected term variable, got $expression"))
  }

  def optionAsVariable(expression: Expression): Option[TermVariable] = {
    expression match {
      case v: TermVariable =>
        Some(v)
      case _ =>
        None
    }
  }

  def parser(implicit context: ExpressionParsingContext): Parser[Term] = {
    Parser.selectWordParser("term") {
      case "with" =>
        for {
          arguments <- Term.parser.listInParensOrSingle(None)
          name <- Parser.singleWord
        } yield context.getTermVariable(name, arguments).getOrElse(throw new Exception(s"Unrecognised statement variable $name"))
      case context.entryContext.RecognisedTermDefinition(termDefinition) =>
        termDefinition.termParser
      case context.entryContext.RecognisedTermShorthand(template) =>
        template.expressionParser.map(_.asInstanceOf[Term])
      case context.SimpleTermVariable(variable) =>
        Parser.constant(variable)
      case context.RecognisedParameter(variableOrParameter) =>
        Parser.constant(variableOrParameter)
    }
  }

  def listParser(implicit context: ExpressionParsingContext): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def variableParser(implicit context: ExpressionParsingContext): Parser[TermVariable] = parser.map(asVariable)

  def variableListParser(implicit context: ExpressionParsingContext): Parser[Seq[TermVariable]] = {
    variableParser.listInParens(None)
  }

  def templateParser(implicit context: TemplateParsingContext): Parser[Template] = {
    Parser.selectWordParser("term template")(templateParserFunction)
  }

  def templateParserFunction(implicit context: TemplateParsingContext): PartialFunction[String, Parser[Template]] = {
    case context.entryContext.RecognisedTermDefinition(definition) =>
      definition.templateParser
    case context.RecognisedParameter(parameter) =>
      Parser.constant(FunctionParameterTemplate(parameter))
    case ExpressionParsingContext.RecognisedTermVariableName(name) =>
      Parser.constant(TermVariableTemplate(name))
  }
}
