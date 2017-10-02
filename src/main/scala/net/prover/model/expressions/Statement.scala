package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Statement extends Assertable {
  def applySubstitutions(substitutions: Substitutions): Option[Statement]
  def replacePlaceholder(other: Expression): Statement
  def calculateApplicatives(arguments: Seq[Term], substitutions: Substitutions, boundVariableCount: Int): Seq[(Predicate, Substitutions)]
  def makeApplicative(argument: Term): Option[Statement]
}

object Statement {
  def parser(implicit context: ParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "_" =>
        Parser.constant(PlaceholderStatement)
      case "with" =>
        for {
          arguments <- Term.parser.listOrSingle(None)
          text <- Parser.singleWord
        } yield PredicateApplication(PredicateVariable(text), arguments)
      case context.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case context.RecognisedStatementVariable(statementVariable) =>
        Parser.constant(statementVariable)
    }
  }

  def listParser(implicit context: ParsingContext): Parser[Seq[Statement]] = parser.listInParens(Some(","))

  def variableParser(implicit context: ParsingContext): Parser[StatementVariable] = parser.map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }
}
