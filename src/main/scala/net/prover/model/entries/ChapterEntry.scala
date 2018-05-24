package net.prover.model.entries

import net.prover.model.{Chapter, Inference}

trait ChapterEntry {
  def inferences: Seq[Inference] = Nil
  def serializedLines: Seq[String]
}

object ChapterEntry {
  trait Key {
    def value: String
    def separator: String
    def chapterKey: Chapter.Key
    def url: String = s"${chapterKey.url}$separator$value"
  }
  object Key {
    case class Standalone(value: String, chapterKey: Chapter.Key) extends Key {
      val separator = "/"
    }
    case class Anchor(value: String, chapterKey: Chapter.Key) extends Key {
      val separator = "#"
    }
  }

  trait WithKey extends ChapterEntry {
    def name: String
    def key: Key
  }
  trait Standalone extends WithKey {
    def key: Key.Standalone
  }
}
