package net.prover.model.entries

import net.prover.books.model.EntryParsingContext
import net.prover.entries.EntryWithContext
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions._

case class StatementDefinitionEntry(
    baseSymbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format.Basic,
    definingStatement: Option[Statement],
    shorthand: Option[String],
    attributes: Seq[String])
  extends ExpressionDefinitionEntry with TypedExpressionDefinitionEntry[StatementDefinitionEntry] with StatementDefinition
{
  override def typeName: String = "Statement"
  override def referencedEntries: Set[ChapterEntry] = definingStatement.map(_.referencedDefinitions).getOrElse(Set.empty).map(_.associatedChapterEntry) - this
  override def inferences: Seq[Inference.FromEntry] = super[StatementDefinition].inferences

  override def withSymbol(newSymbol: String): StatementDefinitionEntry = copy(baseSymbol = newSymbol)
  override def withName(newName: Option[String]): StatementDefinitionEntry = copy(explicitName = newName)
  override def withShorthand(newShorthand: Option[String]): StatementDefinitionEntry = copy(shorthand = newShorthand)
  override def withAttributes(newAttributes: Seq[String]): StatementDefinitionEntry = copy(attributes = newAttributes)
  override def withFormat(newFormat: Format.Basic): StatementDefinitionEntry = copy(format = newFormat)

  override def serializedLines: Seq[String] = Seq(s"statement $baseSymbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.toSeq ++
      definingStatement.map(s => s"definition (${s.serialized})").toSeq ++
      shorthand.map(s => s"shorthand ($s)").toSeq ++
      Attributes.serialize(attributes).toSeq
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): StatementDefinitionEntry = {
    StatementDefinitionEntry(
      baseSymbol,
      boundVariableNames,
      componentTypes,
      explicitName,
      format,
      definingStatement.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      shorthand,
      attributes)
  }
}

object StatementDefinitionEntry extends ChapterEntryParser {
  override val name: String = "statement"

  def nameParser: Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  private def definingStatementParser(implicit context: ExpressionParsingContext): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens)

  def parser(implicit context: EntryParsingContext): Parser[StatementDefinitionEntry] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinitionEntry.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
      name <- nameParser
      format <- Format.optionalParserForExpressionDefinition(symbol, boundVariables, componentTypes)
      optionalDefiningStatement <- definingStatementParser(expressionParsingContext)
      shorthand <- ExpressionDefinitionEntry.shorthandParser
      attributes <- Attributes.parser
    } yield {
      StatementDefinitionEntry(
        symbol,
        boundVariables,
        componentTypes,
        name,
        format,
        optionalDefiningStatement,
        shorthand,
        attributes)
    }
  }
}
