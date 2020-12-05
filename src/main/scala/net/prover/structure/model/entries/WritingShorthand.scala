package net.prover.structure.model.entries

import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.expressions.Template
import net.prover.model.{Inference, Parser}
import net.prover.structure.EntryContext
import net.prover.structure.parsers.ChapterEntryParser

case class WritingShorthand(template: Template, symbol: String) extends ChapterEntry {
  override def name: String = symbol
  override def serializedLines: Seq[String] = Seq(Seq("write", template.serialized, "as", symbol).mkString(" "))
  override def inferences: Seq[Inference.FromEntry] = Nil
  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = template.referencedDefinitions.map(_.associatedChapterEntry)
  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition],
    entryContext: EntryContext
  ): WritingShorthand = copy(template = template.replaceDefinitions(expressionDefinitionReplacements))
}

object WritingShorthand extends ChapterEntryParser {
  override def name: String = "write"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] =
    for {
      template <- Template.parser
      symbol <- Parser.required("as", Parser.singleWord)
    } yield WritingShorthand(template, symbol)
}
