package net.prover.model.components

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Statement extends Component {
  override val componentType = Statement
  def applySubstitutions(substitutions: Substitutions): Option[Statement]
  def replacePlaceholder(other: Component): Statement
  def calculateApplicatives(argument: Term, substitutions: Substitutions): Seq[(Predicate, Substitutions)] = {
    Seq((Predicate.Constant(this), substitutions))
  }
}

object Statement extends ComponentType {

  def parser(implicit context: ParsingContext): Parser[Statement] = {
    Parser.selectWord("statement") {
      case "_" =>
        Parser.constant(PlaceholderStatement)
      case "with" =>
        for {
          argument <- Term.variableParser
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
