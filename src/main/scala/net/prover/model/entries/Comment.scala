package net.prover.model.entries

import net.prover.books.reading.ProofFileReader
import net.prover.entries.EntryWithContext
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.{AvailableEntries, Inference, Parser}

case class Comment(text: String) extends ChapterEntry {
  override def name: String = Comment.name
  override def serializedLines: Seq[String] = Seq(s"comment $text")

  override def inferences: Seq[Inference.FromEntry] = Nil
  override def referencedEntries: Set[ChapterEntry] = Set.empty
  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): Comment = this
}

object Comment extends ChapterEntryParser {
  override val name: String = "comment"
  override def parser(implicit availableEntries: AvailableEntries, proofFileReader: ProofFileReader): Parser[Comment] = {
    for {
      text <- Parser.toEndOfLine
    } yield Comment(text)
  }
}
