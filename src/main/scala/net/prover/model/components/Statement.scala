package net.prover.model.components

import net.prover.model.entries.StatementDefinition
import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Statement extends Component {
  override val componentType = Statement
  def applySubstitutions(substitutions: Substitutions): Option[Statement]
  def replacePlaceholder(other: Component): Option[Statement]
}

object Statement extends ComponentType {

  def parser(implicit context: ParsingContext): Parser[Statement] = {
    object ParsableStatement {
      def unapply(s: String): Option[StatementDefinition] = {
        context.statementDefinitions.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[StatementVariable] = {
        context.statementVariableNames.find(_ == s).map(StatementVariable)
      }
    }

    def parserForStatementType(statementType: String): Parser[Statement] = statementType match {
      case "_" =>
        Parser.constant(PlaceholderStatement)
      case ParsableStatement(statementDefinition) =>
        statementDefinition.statementParser
      case SpecifiedVariable(v) =>
        Parser.constant(v)
      case _ =>
        throw new Exception(s"Unrecognised statement type $statementType")
    }

    Parser.singleWord.flatMap(parserForStatementType)
  }

  def listParser(implicit context: ParsingContext): Parser[Seq[Statement]] = parser.listInParens(Some(","))

  def variableParser(implicit context: ParsingContext): Parser[StatementVariable] = parser.map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }
}
