package net.prover.model.entries

import net.prover.model.{Format, Parser, ParsingContext}
import net.prover.model.expressions.{Expression, Template}

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
