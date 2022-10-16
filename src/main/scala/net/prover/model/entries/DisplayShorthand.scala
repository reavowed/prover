package net.prover.model.entries

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.books.io.EntryParsingContext
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions.Template
import net.prover.model.{EntryContext, Format, Inference, Parser}

@JsonSerialize(using = classOf[DisplayShorthandSerializer])
case class DisplayShorthand(template: Template, format: Format.Explicit, conditions: Seq[(String, String)]) extends ChapterEntry {
  override def name: String = DisplayShorthand.name
  override def serializedLines: Seq[String] = Seq((
    Seq("display", template.serialized, "as", format.serializedWithoutPrefix) ++
      conditions.flatMap { case (variableName, requiredAttribute) => Seq("if", variableName, requiredAttribute) }
    ).mkString(" "))

  override def inferences: Seq[Inference.FromEntry] = Nil
  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = template.referencedDefinitions.map(_.associatedChapterEntry)

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): DisplayShorthand = {
    DisplayShorthand(
      template.replaceDefinitions(expressionDefinitionReplacements),
      format,
      conditions)
  }
}

object DisplayShorthand extends ChapterEntryParser {
  override def name = "display"

  def conditionsParser: Parser[Seq[(String, String)]] = {
    Parser.optional(
      "if",
      for {
        variableName <- Parser.singleWord
        requiredAttribute <- Parser.singleWord
      } yield (variableName, requiredAttribute)
    ).whileDefined
  }

  override def parser(implicit context: EntryParsingContext): Parser[DisplayShorthand] = {
    for {
      template <- Template.parser
      format <- Parser.required("as", Format.parser(template.names))
      conditions <- conditionsParser
    } yield DisplayShorthand(template, format, conditions)
  }
}

private class DisplayShorthandSerializer extends JsonSerializer[DisplayShorthand] {
  override def serialize(value: DisplayShorthand, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartObject(value)
    gen.writeObjectField("baseFormatString", value.format.baseFormatString)
    gen.writeObjectField("requiresBrackets", value.format.requiresBrackets)
    gen.writeObjectField("conditions", value.conditions)
    gen.writeObjectField("template", value.template.serialized)
    gen.writeEndObject()
  }
}
