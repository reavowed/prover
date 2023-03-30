package net.prover.books.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.books.keys.{ListWithKeys, WithKeyProperty}
import net.prover.entries.BookWithContext
import net.prover.model.{Chapter, SeqOps}

@JsonIgnoreProperties(Array("dependencies"))
case class Book(
    title: String,
    imports: Seq[String],
    chaptersWithKeys: ListWithKeys[Chapter])
{
  val chapters: List[Chapter] = chaptersWithKeys.list
  def addChapter(chapter: Chapter): Book = copy(chaptersWithKeys = chaptersWithKeys :+ chapter)
  def updateChapter(key: String, chapter: Chapter) = copy(chaptersWithKeys = chaptersWithKeys.updated(key, chapter))
  def setChapters(chapters: List[Chapter]): Book = copy(chaptersWithKeys = ListWithKeys(chapters))

  def serialized: String = {
    val sections = Seq(
      imports.map(i => s"import $i"),
      chapters.map(c => s"chapter ${c.title}"))

    sections.filter(_.nonEmpty).map(_.mkString("\n")).mkString("\n\n") + "\n"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Book]

  override def equals(other: Any): Boolean = other match {
    case that: Book =>
      (that canEqual this) &&
        title == that.title
    case _ => false
  }

  override def hashCode(): Int = {
    title.hashCode
  }
}

object Book {
  implicit val keyProperty: WithKeyProperty[Book] = _.title
  def getDependencies(imports: Seq[String], availableBooks: Seq[BookWithContext]): Seq[BookWithContext] = {
    imports
      .map { importTitle =>
        availableBooks.find(_.book.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$importTitle'"))
      }
      .flatMap { importedBook =>
        getDependencies(importedBook.book.imports, availableBooks) :+ importedBook
      }
      .distinctBy(_.book.title)
  }
}
