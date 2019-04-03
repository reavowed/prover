package net.prover.model.entries

import net.prover.model.Inference

trait ChapterEntry {
  def name: String
  def inferences: Seq[Inference] = Nil
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def referencedDefinitions: Set[ExpressionDefinition]
}

object ChapterEntry {
  trait Standalone extends ChapterEntry {
    def title: String
  }
}
