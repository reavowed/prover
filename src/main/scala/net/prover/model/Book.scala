package net.prover.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries._

@JsonIgnoreProperties(Array("dependencies"))
case class Book(
    title: String,
    dependencies: Seq[Book],
    chapters: Seq[Chapter]) {
  val key: String = title.formatAsKey

  def inferences: Seq[Inference] = chapters.flatMap(_.inferences)
  def theorems: Seq[Theorem] = chapters.flatMap(_.theorems)
  def statementDefinitions: Seq[StatementDefinition] = chapters.flatMap(_.statementDefinitions)
}

object Book {
  implicit class BookSeqOps(books: Seq[Book]) {
    def transitive: Seq[Book] = {
      (books ++ books.flatMap(_.dependencies.transitive)).distinctBy(_.title)
    }
  }
}