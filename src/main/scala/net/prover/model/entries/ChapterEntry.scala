package net.prover.model.entries

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.{Chapter, Inference}

trait ChapterEntry {
  def inferences: Seq[Inference] = Nil
  def serializedLines: Seq[String]
}

object ChapterEntry {
  @JsonSerialize(using = classOf[KeySerializer])
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

  class KeySerializer extends JsonSerializer[Key] {
    override def serialize(value: Key, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
      gen.writeString(value.url)
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
