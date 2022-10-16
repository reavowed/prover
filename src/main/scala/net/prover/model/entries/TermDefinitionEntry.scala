package net.prover.model.entries

import net.prover.books.io.EntryParsingContext
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ExpressionDefinition, TermDefinition}
import net.prover.model.expressions._

case class TermDefinitionEntry(
    baseSymbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    disambiguator: Option[String],
    explicitName: Option[String],
    format: Format.Basic,
    premises: Seq[Statement],
    definitionPredicate: Statement,
    shorthand: Option[String],
    attributes: Seq[String],
    disambiguatorAdders: Seq[DisambiguatorAdder])
  extends ExpressionDefinitionEntry
    with TypedExpressionDefinitionEntry[TermDefinitionEntry]
    with TermDefinition
    with ChapterEntry.HasDefiningStatement
{
  override def typeName: String = "Term"
  override def referencedEntries: Set[ChapterEntry] = (definingStatement.referencedDefinitions ++ premises.flatMap(_.referencedDefinitions).toSet).map(_.associatedChapterEntry) - this
  override def inferences: Seq[Inference.FromEntry] = super[TermDefinition].inferences

  override def withSymbol(newSymbol: String): TermDefinitionEntry = copy(baseSymbol = newSymbol)
  def withDisambiguator(newDisambiguator: Option[String]): TermDefinitionEntry = copy(disambiguator = newDisambiguator)
  override def withName(newName: Option[String]): TermDefinitionEntry = copy(explicitName = newName)
  override def withShorthand(newShorthand: Option[String]): TermDefinitionEntry = copy(shorthand = newShorthand)
  override def withAttributes(newAttributes: Seq[String]): TermDefinitionEntry = copy(attributes = newAttributes)
  override def withFormat(newFormat: Format.Basic): TermDefinitionEntry = copy(format = newFormat)
  def withDisambiguatorAdders(newDisambiguatorAdders: Seq[DisambiguatorAdder]): TermDefinitionEntry = copy(disambiguatorAdders = newDisambiguatorAdders)
  override def withDefiningStatement(newDefiningStatement: Statement) = copy(definitionPredicate = newDefiningStatement)

  override def definingStatementParsingContext(implicit entryContext: EntryContext) = {
    ExpressionParsingContext.forComponentTypes(componentTypes).addInitialParameter("_")
  }

  override def serializedLines: Seq[String] = Seq(s"term $baseSymbol $serializedComponents") ++
    (disambiguator.map(d => s"disambiguator $d").toSeq ++
      explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.toSeq ++
      (if (premises.nonEmpty) Seq(s"premises (${premises.map(_.serialized).mkString(", ")})") else Nil) ++
      Seq("(" + definitionPredicate.serialized + ")") ++
      shorthand.map(s => s"shorthand ($s)").toSeq ++
      Attributes.serialize(attributes).toSeq ++
      (if (disambiguatorAdders.nonEmpty) Seq("disambiguatorAdders " + disambiguatorAdders.serialized) else Nil)
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): TermDefinitionEntry = {
    TermDefinitionEntry(
      baseSymbol,
      boundVariableNames,
      componentTypes,
      disambiguator,
      explicitName,
      format,
      premises.map(_.replaceDefinitions(expressionDefinitionReplacements)),
      definitionPredicate.replaceDefinitions(expressionDefinitionReplacements),
      shorthand,
      attributes,
      disambiguatorAdders.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }
}

object TermDefinitionEntry extends ChapterEntryParser {
  override val name: String = "term"

  def premisesParser(implicit context: ExpressionParsingContext): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def nameParser: Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  def parser(implicit context: EntryParsingContext): Parser[TermDefinitionEntry] = {
    for {
      baseSymbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinitionEntry.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
      disambiguatorOption <- Parser.optional("disambiguator", Parser.singleWord)
      name <- nameParser
      format <- Format.optionalParserForExpressionDefinition(baseSymbol, boundVariables, componentTypes)
      premises <- premisesParser(expressionParsingContext)
      definitionPredicate <- Statement.parser(expressionParsingContext.addInitialParameter("_")).inParens
      shorthand <- ExpressionDefinitionEntry.shorthandParser
      attributes <- Attributes.parser
      disambiguatorAdders <- Parser.optional("disambiguatorAdders", DisambiguatorAdder.listParser).getOrElse(Nil)
    } yield {
      TermDefinitionEntry(
        baseSymbol,
        boundVariables,
        componentTypes,
        disambiguatorOption,
        name,
        format,
        premises,
        definitionPredicate,
        shorthand,
        attributes,
        disambiguatorAdders)
    }
  }
}

