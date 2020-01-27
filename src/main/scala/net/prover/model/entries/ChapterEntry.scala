package net.prover.model.entries

import net.prover.model.{EntryContext, Inference}

trait ChapterEntry {
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
    def title: String
  }
}
