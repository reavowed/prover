package net.prover.structure.parsers

import net.prover.core.expressions.{CompoundTerm, Term}
import net.prover.model.definitions.CompoundTermDefinition
import net.prover.model.expressions.{CompoundTermTemplate, TermVariableTemplate}
import net.prover.model.template.{CompoundTermTemplate, TermTemplate, TermVariableTemplate}
import net.prover.model.{ExpressionParsingContext, Parser, TemplateParsingContext}
import net.prover.structure.parsers.CompoundExpressionComponentParsers._

object TermParsers {
  def termParser(implicit context: ExpressionParsingContext): Parser[Term] = {
    Parser.selectWordParser("term") {
      case "with" =>
        for {
          arguments <- termParser.listInParensOrSingle(None)
          name <- Parser.singleWord
        } yield context.getTermVariable(name, arguments).getOrElse(throw new Exception(s"Unrecognised statement variable $name"))
      case context.entryContext.RecognisedCompoundTermDefinition(termDefinition) =>
        compoundTermParser(termDefinition)
        termDefinition.termParser
      case context.entryContext.RecognisedTermShorthand(template) =>
        template.expressionParser.map(_.asInstanceOf[Term])
      case context.SimpleTermVariable(variable) =>
        Parser.constant(variable)
      case context.RecognisedParameter(variableOrParameter) =>
        Parser.constant(variableOrParameter)
    }
  }

  def compoundTermParser(compoundTermDefinition: CompoundTermDefinition)(implicit context: ExpressionParsingContext): Parser[CompoundTerm] = {
    boundVariableNamesAndComponentExpressionsParser(compoundTermDefinition).map { case (newBoundVariableNames, components) =>
      CompoundTerm(compoundTermDefinition, components)(newBoundVariableNames)
    }
  }

  def termTemplateParser(implicit context: TemplateParsingContext): Parser[TermTemplate] = {
    Parser.selectWordParser("term template")(termTemplateParserFunction)
  }

  def termTemplateParserFunction(implicit context: TemplateParsingContext): PartialFunction[String, Parser[TermTemplate]] = {
    case context.entryContext.RecognisedCompoundTermDefinition(definition) =>
      compoundTermTemplateParser(definition)
    case context.RecognisedParameter(parameter) =>
      Parser.constant(ParameterTemplate(parameter))
    case ExpressionParsingContext.RecognisedTermVariableName(name) =>
      Parser.constant(TermVariableTemplate(name))
  }

  def compoundTermTemplateParser(compoundTermDefinition: CompoundTermDefinition)(implicit context: TemplateParsingContext): Parser[CompoundTermTemplate] = {
    boundVariableNamesAndComponentTemplateParser(compoundTermDefinition).map { case (boundVariableNames, componentTemplates) =>
      CompoundTermTemplate(compoundTermDefinition, boundVariableNames, componentTemplates)
    }
  }
}
