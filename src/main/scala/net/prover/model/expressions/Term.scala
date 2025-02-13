package net.prover.model.expressions

import net.prover.model.entries.WritingShorthand
import net.prover.model.{ExpressionParsingContext, Substitutions, TemplateParsingContext}
import net.prover.parsing.{KnownWordParser, Parser}

trait Term extends Expression with TypedExpression[Term] {
  override def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, Term, Int, Seq[Int])] = Seq((this, FunctionParameter(0, internalDepth + externalDepth), internalDepth, Nil))
  def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(Term, Substitutions.Possible)] = {
    for {
      (argument, index) <- baseArguments.zipWithIndex.iterator
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

  def parser(implicit context: ExpressionParsingContext): KnownWordParser[Term] = {
    KnownWordParser.select(Seq(
      TermVariable.simpleParser,
      TermVariable.applicationParser,
      DefinedTerm.parser,
      WritingShorthand.termParser,
      FunctionParameter.parser))
  }

  def listParser(implicit context: ExpressionParsingContext): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def templateParser(implicit context: TemplateParsingContext): Parser[Template] = {
    Parser.selectWordParser("term template")(templateParserFunction)
  }

  def templateParserFunction(implicit context: TemplateParsingContext): PartialFunction[String, Parser[Template]] = {
    case context.availableEntries.RecognisedTermDefinition(definition) =>
      definition.templateParser
    case context.RecognisedParameter(parameter) =>
      Parser.constant(FunctionParameterTemplate(parameter))
    case ExpressionParsingContext.RecognisedTermVariableName(name) =>
      Parser.constant(TermVariableTemplate(name))
  }
}
