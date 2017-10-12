package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Statement extends Expression {
  def increaseDepth(additionalDepth: Int): Statement
  def reduceDepth(difference: Int): Option[Statement]
  def specify(arguments: Seq[Term]): Statement
  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    outerDepth: Int
  ): Option[Statement]
  def applySubstitutions(substitutions: Substitutions): Option[Statement]
  def calculateApplicatives(baseArguments: Seq[Term], substitutions: Substitutions): Seq[(Statement, Substitutions)]
}

object Statement {
  def parser(implicit context: ParsingContext): Parser[Statement] = {
    Parser.selectWordParser("statement") {
      case "with" =>
        for {
          arguments <- Term.parser.listOrSingle(None)
          name <- Parser.singleWord
        } yield PredicateApplication(name, arguments, context.parameterDepth)
      case context.RecognisedStatementDefinition(statementDefinition) =>
        statementDefinition.statementParser
      case context.RecognisedStatementVariable(name) =>
        Parser.constant(StatementVariable(name, context.parameterDepth))
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
