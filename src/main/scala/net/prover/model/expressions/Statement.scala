package net.prover.model.expressions

import net.prover.model._

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
        typeStatementParser
      case context.entryContext.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
        Parser.constant(StatementVariable(name))
    }
  }

  def typeStatementParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    for {
      term <- Term.parser
      typeDefinitionSymbol <- Parser.singleWord
      typeDefinition = context.entryContext.typeDefinitions.find(_.symbol == typeDefinitionSymbol).getOrElse(throw new Exception(s"Unrecognised type '$typeDefinitionSymbol'"))
      otherComponents <- typeDefinition.otherComponentTypes.map(_.expressionParser).traverseParser
      availableProperties = context.entryContext.propertyDefinitionsByType.get(typeDefinitionSymbol).toSeq.flatten
      properties <- Parser.optional("with", Parser.allInParens.map(_.splitByWhitespace().map(s => availableProperties.find(_.symbol == s).getOrElse(throw new Exception(s"Unrecognised property '$s' for '$typeDefinitionSymbol'")))), Nil)
    } yield {
      val baseStatement = DefinedStatement(term +: otherComponents, typeDefinition.statementDefinition)(Nil)
      properties.foldLeft(baseStatement) { (statement, property) =>
        val conjunctionDefinition = context.entryContext.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot add properties to a type without a conjunction definition"))
        val propertyStatement = DefinedStatement(term +: otherComponents, property.statementDefinition)(Nil)
        DefinedStatement(Seq(statement, propertyStatement), conjunctionDefinition)(Nil)
      }
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
