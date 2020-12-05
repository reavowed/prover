package net.prover.structure.model.entries

import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.{Inference, Parser}
import net.prover.structure.EntryContext
import net.prover.structure.parsers.ChapterEntryParser

case class Comment(text: String) extends ChapterEntry {
  override def name: String = Comment.name
  override def serializedLines: Seq[String] = Seq(s"comment $text")

  override def inferences: Seq[Inference.FromEntry] = Nil
  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = Set.empty
  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition],
    entryContext: EntryContext
  ): Comment = this
}

object Comment extends ChapterEntryParser {
  override val name: String = "comment"
  override def parser(implicit context: EntryContext): Parser[Comment] = {
    for {
      text <- Parser.toEndOfLine
    } yield Comment(text)
  }
}
