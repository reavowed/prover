package net.prover.model.expressions

import net.prover.model.{ExpressionParsingContext, Parser, Substitutions}

trait Term extends Expression with TypedExpression[Term] {
  def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Seq[(Term, Substitutions)] = {
    for {
      (argument, index) <- baseArguments.zipWithIndex
      updatedSubstitutions <- argument
        .insertExternalParameters(internalDepth)
        .calculateSubstitutions(this, substitutions, previousInternalDepth + internalDepth, externalDepth)
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
          arguments <- Term.parser.listOrSingle(None)
          name <- Parser.singleWord
        } yield FunctionApplication(name, arguments)
      case context.RecognisedParameter(parameter) =>
        Parser.constant(parameter)
      case context.RecognisedTermDefinition(termDefinition) =>
        termDefinition.termParser
      case context.RecognisedTermVariable(name) =>
        Parser.constant(TermVariable(name))
    }
  }

  def listParser(implicit context: ExpressionParsingContext): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def variableParser(implicit context: ExpressionParsingContext): Parser[TermVariable] = parser.map(asVariable)

  def variableListParser(implicit context: ExpressionParsingContext): Parser[Seq[TermVariable]] = {
    variableParser.listInParens(None)
  }

  def templateParser(implicit context: ExpressionParsingContext): Parser[Template] = {
    Parser.selectWordParser("term template") {
      case context.RecognisedTermVariable(name) =>
        Parser.constant(Template.TermVariable(name))
      case context.RecognisedTermDefinition(definition) =>
        definition.templateParser
      case context.RecognisedParameter(parameter) =>
        Parser.constant(Template.FunctionParameter(parameter))
    }
  }
}
