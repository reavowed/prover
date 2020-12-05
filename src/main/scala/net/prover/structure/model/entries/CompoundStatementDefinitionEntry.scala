package net.prover.structure.model.entries

import net.prover._
import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType
import net.prover.model.definitions.{CompoundExpressionDefinition, CompoundStatementDefinition}
import net.prover.model.expressions._
import net.prover.structure.EntryContext
import net.prover.structure.parsers.ChapterEntryParser

case class CompoundStatementDefinitionEntry(
    baseSymbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format.Basic,
    definingStatement: Option[Statement],
    shorthand: Option[String],
    attributes: Seq[String])
  extends CompoundExpressionDefinitionEntry with TypedExpressionDefinitionEntry[CompoundStatementDefinitionEntry] with CompoundStatementDefinition
{
  override def typeName: String = "Statement"
  override def referencedEntries: Set[ChapterEntry] = definingStatement.map(_.referencedDefinitions).getOrElse(Set.empty).map(_.associatedChapterEntry) - this
  override def inferences: Seq[Inference.FromEntry] = super[CompoundStatementDefinition].inferences

  override def withSymbol(newSymbol: String): CompoundStatementDefinitionEntry = copy(baseSymbol = newSymbol)
  override def withName(newName: Option[String]): CompoundStatementDefinitionEntry = copy(explicitName = newName)
  override def withShorthand(newShorthand: Option[String]): CompoundStatementDefinitionEntry = copy(shorthand = newShorthand)
  override def withAttributes(newAttributes: Seq[String]): CompoundStatementDefinitionEntry = copy(attributes = newAttributes)
  override def withFormat(newFormat: Format.Basic): CompoundStatementDefinitionEntry = copy(format = newFormat)

  override def serializedLines: Seq[String] = Seq(s"statement $baseSymbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.toSeq ++
      definingStatement.map(s => s"definition (${s.serialized})").toSeq ++
      shorthand.map(s => s"shorthand ($s)").toSeq ++
      Attributes.serialize(attributes).toSeq
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition],
    entryContext: EntryContext
  ): CompoundStatementDefinitionEntry = {
    CompoundStatementDefinitionEntry(
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

object CompoundStatementDefinitionEntry extends ChapterEntryParser {
  override val name: String = "statement"

  def nameParser: Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  private def definingStatementParser(implicit context: ExpressionParsingContext): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens)

  def parser(implicit entryContext: EntryContext): Parser[CompoundStatementDefinitionEntry] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- CompoundExpressionDefinitionEntry.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
      name <- nameParser
      format <- Format.optionalParserForExpressionDefinition(symbol, boundVariables, componentTypes)
      optionalDefiningStatement <- definingStatementParser(expressionParsingContext)
      shorthand <- CompoundExpressionDefinitionEntry.shorthandParser
      attributes <- Attributes.parser
    } yield {
      CompoundStatementDefinitionEntry(
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
