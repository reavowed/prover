package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.entries.ChapterEntry.Key
import net.prover.model.{Chapter, Inference}

trait ChapterEntry {
  def name: String
  def key: Key
  def inferences: Seq[Inference] = Nil
  def serializedLines: Seq[String]
}

object ChapterEntry {
  sealed trait Key {
    def name: String
    def value: String
    @JsonIgnore
    def separator: String
    def chapterKey: Chapter.Key
    @JsonSerialize
    def url: String = s"${chapterKey.url}$separator$value"
  }
  object Key {
    case class Standalone(name: String, value: String, chapterKey: Chapter.Key) extends Key {
      val separator = "/"
    }
    object Standalone {
      def apply(name: String, getValueAndKey: String => (String, Chapter.Key)): Standalone = {
        val (value, key) = getValueAndKey(name)
        Standalone(name, value, key)
      }
    }
    case class Anchor(name: String, value: String, chapterKey: Chapter.Key) extends Key {
      val separator = "#"
    }
    object Anchor {
      def apply(name: String, getValueAndKey: String => (String, Chapter.Key)): Anchor = {
        val (value, key) = getValueAndKey(name)
        Anchor(name, value, key)
      }
    }
  }

  class KeySerializer extends JsonSerializer[Key] {
    override def serialize(value: Key, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
      gen.writeString(value.url)
    }
  }

  trait Standalone extends ChapterEntry {
    def key: Key.Standalone
    def title: String
  }
}
