package net.prover.model.entries

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.expressions.Template
import net.prover.model.{EntryContext, ExpressionParsingContext, Format, Parser}

@JsonSerialize(using = classOf[DisplayShorthandSerializer])
case class DisplayShorthand(template: Template, format: Format.Explicit, conditions: Seq[(String, String)]) extends ChapterEntry {
  override def name: String = DisplayShorthand.name
  override def serializedLines: Seq[String] = Seq((
    Seq("display", template.serialized, "as", s"(${format.originalValue})") ++
      conditions.flatMap { case (variableName, requiredAttribute) => Seq("if", variableName, requiredAttribute) }
    ).mkString(" "))

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
}

object DisplayShorthand extends ChapterEntryParser {
  override def name = "display"

  def conditionsParser: Parser[Seq[(String, String)]] = {
    Parser.optionalWord("if").flatMapMap { _ =>
      for {
        variableName <- Parser.singleWord
        requiredAttribute <- Parser.singleWord
      } yield (variableName, requiredAttribute)
    }.whileDefined
  }

  override def parser(implicit entryContext: EntryContext): Parser[DisplayShorthand] = {
    for {
      template <- Template.parser
      _ <- Parser.requiredWord("as")
      format <- Format.parser(template.names)
      conditions <- conditionsParser
    } yield DisplayShorthand(template, format, conditions)
  }
}

private class DisplayShorthandSerializer extends JsonSerializer[DisplayShorthand] {
  private def serialize(template: Template, gen: JsonGenerator): Unit = {
    template match {
      case Template.StatementVariable(name) =>
        gen.writeString(name)
      case Template.TermVariable(name) =>
        gen.writeString(name)
      case Template.DefinedStatement(definition, boundVariableNames, components) =>
        gen.writeStartArray(boundVariableNames.length + components.length + 1)
        gen.writeString(definition.symbol)
        components.foreach(serialize(_, gen))
        gen.writeEndArray()
      case Template.DefinedTerm(definition, boundVariableNames, components) =>
        gen.writeStartArray(boundVariableNames.length + components.length + 1)
        gen.writeString(definition.symbol)
        components.foreach(serialize(_, gen))
        gen.writeEndArray()
      case Template.FunctionParameter(parameter) =>
        gen.writeStartArray(2)
        gen.writeNumber(parameter.level)
        gen.writeNumber(parameter.index)
        gen.writeEndArray()
    }
  }

  override def serialize(value: DisplayShorthand, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeStartObject(value)
    gen.writeObjectField("baseFormatString", value.format.baseFormatString)
    gen.writeObjectField("requiresBrackets", value.format.requiresBrackets)
    gen.writeObjectField("conditions", value.conditions)
    gen.writeFieldName("template")
    serialize(value.template, gen)
    gen.writeEndObject()
  }
}
