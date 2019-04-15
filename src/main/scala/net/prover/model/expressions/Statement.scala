package net.prover.model.expressions

import net.prover.model.{ExpressionParsingContext, Parser, TemplateParsingContext}

trait Statement extends Expression with TypedExpression[Statement]

object Statement {
  def parser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "with" =>
        for {
          arguments <- Term.parser.listOrSingle(None)
          name <- Parser.singleWord
        } yield PredicateApplication(name, arguments)
      case "is" =>
        for {
          term <- Term.parser
          typeDefinitionSymbol <- Parser.singleWord
          typeDefinition = context.entryContext.typeDefinitions.find(_.symbol == typeDefinitionSymbol).getOrElse(throw new Exception(s"Unrecognised type definition '$typeDefinitionSymbol'"))
          otherComponents <- typeDefinition.otherComponentTypes.map(_.expressionParser).traverseParser
        } yield DefinedStatement(term +: otherComponents, typeDefinition.statementDefinition)(Nil)
      case context.entryContext.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
        Parser.constant(StatementVariable(name))
    }
  }

  def listParser(implicit context: ExpressionParsingContext): Parser[Seq[Statement]] = parser.listInParens(Some(","))

  def variableParser(implicit context: ExpressionParsingContext): Parser[StatementVariable] = parser.map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }

  def templateParser(implicit templateParsingContext: TemplateParsingContext): Parser[Template] = {
    Parser.selectWordParser("statement template")(templateParserFunction)
  }

  def templateParserFunction(implicit templateParsingContext: TemplateParsingContext): PartialFunction[String, Parser[Template]] = {
      case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
        Parser.constant(Template.StatementVariable(name))
      case templateParsingContext.entryContext.RecognisedStatementDefinition(definition) =>
        definition.templateParser
  }
}
