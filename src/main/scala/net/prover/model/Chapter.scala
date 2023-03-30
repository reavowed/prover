package net.prover.model

import net.prover.books.keys.{ListWithKeys, WithKeyProperty}
import net.prover.model.entries._

case class Chapter(
    title: String,
    summary: String,
    entriesWithKeys: ListWithKeys[ChapterEntry])
{
  val entries: List[ChapterEntry] = entriesWithKeys.list
  def serialized: String = {
    val entryTexts = entries.map(_.serializedLines.mkString("\n"))
    (summary +: entryTexts).mkString("\n\n") + "\n"
  }

  def addEntry(newEntry: ChapterEntry): Chapter = copy(entriesWithKeys = entriesWithKeys :+ newEntry)
  def updateEntry(key: String, entry: ChapterEntry) = copy(entriesWithKeys = entriesWithKeys.updated(key, entry))
  def setEntries(entries: List[ChapterEntry]): Chapter = copy(entriesWithKeys = ListWithKeys(entries))

  def canEqual(other: Any): Boolean = other.isInstanceOf[Chapter]

  override def equals(other: Any): Boolean = other match {
    case that: Chapter =>
      (that canEqual this) &&
        title == that.title
    case _ => false
  }

  override def hashCode(): Int = {
    title.hashCode
  }
}

object Chapter {
  implicit val keyProperty: WithKeyProperty[Chapter] = _.title
}
