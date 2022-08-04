package net.prover.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties

@JsonIgnoreProperties(Array("dependencies"))
case class Book(
    title: String,
    imports: Seq[String],
    chapters: Seq[Chapter])
{
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
  def getDependencies(imports: Seq[String], availableBooks: Seq[Book]): Seq[Book] = {
    imports
      .map { importTitle =>
        availableBooks.find(_.title == importTitle).getOrElse(throw new Exception(s"Could not find imported book '$importTitle'"))
      }
      .flatMap { importedBook =>
        getDependencies(importedBook.imports, availableBooks) :+ importedBook
      }
      .distinctBy(_.title)
  }
}
