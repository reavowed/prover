package net.prover.model.entries

import net.prover.model.{Format, Parser, ParsingContext}
import net.prover.model.expressions.{Expression, Template}

case class Shorthand(template: Template, format: Format.Explicit) extends ChapterEntry {
  override def serializedLines: Seq[String] = Seq(s"shorthand ${template.serialized} as (${format.originalValue})")
}

object Shorthand extends ChapterEntryParser.WithoutKey {
  override def name = "shorthand"
  override def parser(chapterTitle: String, bookTitle: String)(implicit context: ParsingContext) = {
    for {
      template <- Template.parser
      _ <- Parser.requiredWord("as")
      format <- Format.parser(template.names)
    } yield Shorthand(template, format)
  }
}