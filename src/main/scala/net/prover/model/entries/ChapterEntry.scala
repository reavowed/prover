package net.prover.model.entries

import net.prover.model.Inference

trait ChapterEntry {
  def inferences: Seq[Inference] = Nil
  def serializedLines: Seq[String]
}

object ChapterEntry {
  trait SelfOutline extends ChapterEntry with ChapterEntryOutline
}