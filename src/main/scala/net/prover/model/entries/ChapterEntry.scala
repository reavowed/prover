package net.prover.model.entries

import net.prover.model.{Chapter, Inference}

trait ChapterEntry {
  def inferences: Seq[Inference] = Nil
  def serializedLines: Seq[String]
}

object ChapterEntry {
  case class Key(value: String, chapterKey: Chapter.Key) {
    def url = s"${chapterKey.url}/$value"
  }

  trait WithKey extends ChapterEntry {
    def name: String
    def key: Key
  }
}
