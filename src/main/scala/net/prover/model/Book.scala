package net.prover.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries._

@JsonIgnoreProperties(Array("dependencies"))
case class Book(
    title: String,
    dependencies: Seq[Book],
    chapters: Seq[Chapter]) {
  val key: String = title.formatAsKey
  implicit def displayContext: DisplayContext = DisplayContext(allTransitive(_.shorthands))

  def inferences: Seq[Inference] = chapters.flatMap(_.inferences)
  def theorems: Seq[Theorem] = chapters.flatMap(_.theorems)
  def statementDefinitions: Seq[StatementDefinition] = chapters.flatMap(_.statementDefinitions)
  def shorthands: Seq[Shorthand] = chapters.flatMap(_.shorthands)

  def allTransitive[T](f: Book => Seq[T]): Seq[T] = (dependencies.transitive :+ this).flatMap(f)
}

object Book {
  implicit class BookSeqOps(books: Seq[Book]) {
    def transitive: Seq[Book] = {
      (books ++ books.flatMap(_.dependencies.transitive)).distinctBy(_.title)
    }
  }
}