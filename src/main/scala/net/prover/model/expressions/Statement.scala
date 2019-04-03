package net.prover.model.expressions

import net.prover.model.{Parser, ExpressionParsingContext}

trait Statement extends Expression with TypedExpression[Statement]

object Statement {
  def parser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "with" =>
        for {
          arguments <- Term.parser.listOrSingle(None)
          name <- Parser.singleWord
        } yield PredicateApplication(name, arguments)
      case context.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case context.RecognisedStatementVariable(name) =>
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

  def templateParser(implicit context: ExpressionParsingContext): Parser[Template] = {
    Parser.selectWordParser("statement template") {
      case context.RecognisedStatementVariable(name) =>
        Parser.constant(Template.StatementVariable(name))
      case context.RecognisedStatementDefinition(definition) =>
        definition.templateParser
    }
  }
}
