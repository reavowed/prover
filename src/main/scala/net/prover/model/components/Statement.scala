package net.prover.model.components

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}
import net.prover.model.entries.StatementDefinition
import net.prover.model.{DistinctVariables, Parser, ParsingContext, Substitutions}

trait Statement extends JsonSerializable.Base with Component {
  override val componentType = Statement
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
  def applySubstitutions(substitutions: Substitutions): Option[Statement]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Statement]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Statement, DistinctVariables)]
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
      case "sub" =>
        for {
          termToReplaceWith <- Term.parser
          termToBeReplaced <- Term.variableParser
          statement <- parser
        } yield {
          statement.makeSingleSubstitution(termToReplaceWith, termToBeReplaced, DistinctVariables.empty)
            .getOrElse(throw new Exception("Invalid substitution"))
        }
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
