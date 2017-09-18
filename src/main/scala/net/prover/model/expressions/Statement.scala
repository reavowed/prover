package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Statement extends Expression {
  override val expressionType = Statement
  def applySubstitutions(substitutions: Substitutions): Option[Statement]
  def replacePlaceholder(other: Expression): Statement
  def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int): Seq[(Predicate, Substitutions)]
  def makeApplicative(argument: Term): Option[Statement]
}

object Statement extends ExpressionType {

  def parser(implicit context: ParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "_" =>
        Parser.constant(PlaceholderStatement)
      case "with" =>
        for {
          argument <- Term.parser
          text <- Parser.singleWord
        } yield PredicateApplication(text, argument)
      case context.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case context.RecognisedStatementVariable(statementVariable) =>
        Parser.constant(statementVariable)
    }
  }
  def applicativeParser(implicit context: ParsingContext) = Predicate.parser

  def listParser(implicit context: ParsingContext): Parser[Seq[Statement]] = parser.listInParens(Some(","))

  def variableParser(implicit context: ParsingContext): Parser[StatementVariable] = parser.map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }
}
