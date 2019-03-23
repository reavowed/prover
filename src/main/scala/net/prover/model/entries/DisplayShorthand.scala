package net.prover.model.entries

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.expressions.Template
import net.prover.model.{Format, Parser, ParsingContext}

@JsonSerialize(using = classOf[DisplayShorthandSerializer])
case class DisplayShorthand(template: Template, format: Format.Explicit) extends ChapterEntry {
  override def serializedLines: Seq[String] = Seq(s"display ${template.serialized} as (${format.originalValue})")
}

object DisplayShorthand extends ChapterEntryParser.WithoutKey {
  override def name = "display"
  override def parser(implicit context: ParsingContext) = {
    for {
      template <- Template.parser
      _ <- Parser.requiredWord("as")
      format <- Format.parser(template.names)
    } yield DisplayShorthand(template, format)
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
    gen.writeFieldName("template")
    serialize(value.template, gen)
    gen.writeEndObject()
  }
}
