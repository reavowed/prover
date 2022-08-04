package net.prover.model

import net.prover.model.entries._

case class Chapter(
    title: String,
    summary: String,
    entries: Seq[ChapterEntry])
{
  def serialized: String = {
    val entryTexts = entries.map(_.serializedLines.mkString("\n"))
    (summary +: entryTexts).mkString("\n\n") + "\n"
  }

  def addEntry(newEntry: ChapterEntry): Chapter = copy(entries = entries :+ newEntry)

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
