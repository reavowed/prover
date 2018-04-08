package net.prover.model.entries

import net.prover.model.Inference

trait ChapterEntry {
  def inferences: Seq[Inference] = Nil
}

object ChapterEntry {
  trait SelfOutline extends ChapterEntry with ChapterEntryOutline
}