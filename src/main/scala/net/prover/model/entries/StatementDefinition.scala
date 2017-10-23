package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries.StatementDefinition.StructureType
import net.prover.model.expressions._
import net.prover.model.{Format, Inference, Parser, ParsingContext}

@JsonIgnoreProperties(Array("symbol", "defaultVariables", "format"))
case class StatementDefinition(
    symbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    name: String,
    format: Format,
    definingStatement: Option[Statement],
    chapterKey: String,
    bookKey: String,
    structureType: Option[StructureType])
  extends ChapterEntry(StatementDefinition)
    with ExpressionDefinition
{
  val defaultValue = {
    DefinedStatement(
      componentTypes.map(_.expression),
      this,
      0)(
      boundVariableNames)
  }

  def statementParser(implicit context: ParsingContext): Parser[Statement] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map(_.expressionParser(newBoundVariableNames)).traverseParser
    } yield DefinedStatement(components, this, context.parameterDepth)(newBoundVariableNames)
  }

  override def inferences: Seq[Inference] = {
    definingStatement.toSeq.flatMap { s =>
      Seq(
        Inference.Definition(name, chapterKey, bookKey, Seq(s), defaultValue),
        Inference.Definition(name, chapterKey, bookKey, Seq(defaultValue), s))
    }
  }
}

object StatementDefinition extends ChapterEntryParser[StatementDefinition] {
  override val name: String = "statement"

  sealed trait StructureType
  object StructureType {
    case object Deduction extends StructureType
    case object Scoping extends StructureType
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


  def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinition.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      name <- nameParser.getOrElse(symbol)
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
        chapterKey,
        bookKey,
        structureType)
    }
  }
}
