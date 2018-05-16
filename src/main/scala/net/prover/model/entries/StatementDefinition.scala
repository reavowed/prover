package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries.StatementDefinition.StructureType
import net.prover.model.expressions._
import net.prover.model._

@JsonIgnoreProperties(Array("symbol", "defaultVariables", "format"))
case class StatementDefinition(
    symbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format,
    definingStatement: Option[Statement],
    structureType: Option[StructureType])
  extends ChapterEntry
    with ExpressionDefinition
{
  def name = explicitName.getOrElse(symbol)

  val defaultValue = {
    DefinedStatement(componentTypes.map(_.expression), this)(boundVariableNames)
  }

  def statementParser(implicit context: ParsingContext): Parser[Statement] = {
    componentExpressionParser.map { case (newBoundVariableNames, components) =>
      DefinedStatement(components, this)(newBoundVariableNames)
    }
  }

  def templateParser(implicit context: ParsingContext): Parser[Template] = {
    componentTemplateParser.map { case (newBoundVariableNames, components) =>
      Template.DefinedStatement(this, newBoundVariableNames, components)
    }
  }

  override def inferences: Seq[Inference] = {
    definingStatement.toSeq.flatMap { s =>
      Seq(
        Inference.Definition(name, Seq(s), defaultValue),
        Inference.Definition(name, Seq(defaultValue), s))
    }
  }

  override def serializedLines: Seq[String] = Seq(s"statement $symbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.map(f => s"format ($f)").toSeq ++
      definingStatement.map(s => s"definition (${s.serialized})").toSeq ++
      structureType.map(_.serialized).toSeq).indent
}

object StatementDefinition extends ChapterEntryParser.WithoutKey {
  override val name: String = "statement"

  sealed trait StructureType {
    def serialized: String
  }
  object StructureType {
    case object Deduction extends StructureType {
      def serialized = "deduction"
    }
    case object Scoping extends StructureType {
      def serialized = "scoping"
    }
    def parser: Parser[Option[StructureType]] = Parser.selectOptionalWord {
      case "deduction" => Deduction
      case "scoping" => Scoping
    }
  }

  def nameParser(implicit context: ParsingContext): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  private def definingStatementParser(implicit context: ParsingContext): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens)

  def parser(implicit context: ParsingContext): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinition.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      name <- nameParser
      format <- Format.optionalParser(symbol, boundVariables ++ componentTypes.map(_.name))
      optionalDefiningStatement <- definingStatementParser
      structureType <- StructureType.parser
    } yield {
      StatementDefinition(
        symbol,
        boundVariables,
        componentTypes,
        name,
        format,
        optionalDefiningStatement,
        structureType)
    }
  }
}
