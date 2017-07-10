package net.prover.model.entries

import net.prover.model.Inference

abstract class ChapterEntry(chapterEntryParser: ChapterEntryParser[_]) {
  val `type`: String = chapterEntryParser.name
  def inferences: Seq[Inference] = Nil
}
