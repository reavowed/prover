package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.{EntryContext, Inference}

trait ChapterEntry {
  @JsonSerialize
  def name: String
  def inferences: Seq[Inference.FromEntry] = Nil
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def referencedDefinitions: Set[ChapterEntry]

  def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): ChapterEntry
}

object ChapterEntry {
  trait Standalone extends ChapterEntry {
    @JsonSerialize
    def title: String
  }
}
