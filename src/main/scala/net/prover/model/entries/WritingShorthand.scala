package net.prover.model.entries

import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.{EntryContext, Parser}
import net.prover.model.expressions.Template

case class WritingShorthand(template: Template, symbol: String) extends ChapterEntry {
  override def name: String = symbol
  override def serializedLines: Seq[String] = Seq(Seq("write", template.serialized, "as", symbol).mkString(" "))
  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = template.referencedDefinitions.map(_.associatedChapterEntry)
  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): WritingShorthand = copy(template = template.replaceDefinition(oldDefinition, newDefinition))
}

object WritingShorthand extends ChapterEntryParser {
  override def name: String = "write"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] =
    for {
      template <- Template.parser
      symbol <- Parser.required("as", Parser.singleWord)
    } yield WritingShorthand(template, symbol)
}
