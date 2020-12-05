package net.prover.structure.model.entries

import net.prover._
import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType
import net.prover.model.definitions.{CompoundExpressionDefinition, CompoundTermDefinition}
import net.prover.model.expressions._
import net.prover.structure.EntryContext
import net.prover.structure.parsers.ChapterEntryParser

case class CompoundTermDefinitionEntry(
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
  extends CompoundExpressionDefinitionEntry with TypedExpressionDefinitionEntry[CompoundTermDefinitionEntry] with CompoundTermDefinition
{
  override def typeName: String = "Term"
  override def referencedEntries: Set[ChapterEntry] = (definingStatement.referencedDefinitions ++ premises.flatMap(_.referencedDefinitions).toSet).map(_.associatedChapterEntry) - this
  override def inferences: Seq[Inference.FromEntry] = super[CompoundTermDefinition].inferences

  override def withSymbol(newSymbol: String): CompoundTermDefinitionEntry = copy(baseSymbol = newSymbol)
  def withDisambiguator(newDisambiguator: Option[String]): CompoundTermDefinitionEntry = copy(disambiguator = newDisambiguator)
  override def withName(newName: Option[String]): CompoundTermDefinitionEntry = copy(explicitName = newName)
  override def withShorthand(newShorthand: Option[String]): CompoundTermDefinitionEntry = copy(shorthand = newShorthand)
  override def withAttributes(newAttributes: Seq[String]): CompoundTermDefinitionEntry = copy(attributes = newAttributes)
  override def withFormat(newFormat: Format.Basic): CompoundTermDefinitionEntry = copy(format = newFormat)
  def withDisambiguatorAdders(newDisambiguatorAdders: Seq[DisambiguatorAdder]): CompoundTermDefinitionEntry = copy(disambiguatorAdders = newDisambiguatorAdders)

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
    expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition],
    entryContext: EntryContext
  ): CompoundTermDefinitionEntry = {
    CompoundTermDefinitionEntry(
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

object CompoundTermDefinitionEntry extends ChapterEntryParser {
  override val name: String = "term"

  def premisesParser(implicit context: ExpressionParsingContext): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def nameParser: Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  def parser(implicit entryContext: EntryContext): Parser[CompoundTermDefinitionEntry] = {
    for {
      baseSymbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- CompoundExpressionDefinitionEntry.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
      disambiguatorOption <- Parser.optional("disambiguator", Parser.singleWord)
      name <- nameParser
      format <- Format.optionalParserForExpressionDefinition(baseSymbol, boundVariables, componentTypes)
      premises <- premisesParser(expressionParsingContext)
      definitionPredicate <- Statement.parser(expressionParsingContext.addInitialParameter("_")).inParens
      shorthand <- CompoundExpressionDefinitionEntry.shorthandParser
      attributes <- Attributes.parser
      disambiguatorAdders <- Parser.optional("disambiguatorAdders", DisambiguatorAdder.listParser).getOrElse(Nil)
    } yield {
      CompoundTermDefinitionEntry(
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

