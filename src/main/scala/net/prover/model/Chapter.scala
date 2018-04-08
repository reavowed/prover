package net.prover.model

import net.prover.model.entries._
import net.prover.model.proof.{CachedProof, ProofEntries}

case class Chapter(
    title: String,
    summary: String,
    bookTitle: String,
    entries: Seq[ChapterEntry] = Nil)
{
  val key: String = title.formatAsKey
  val bookKey: String = bookTitle.formatAsKey

  def statementDefinitions: Seq[StatementDefinition] = {
    entries.ofType[StatementDefinition]
  }

  def inferences: Seq[Inference] = {
    entries.flatMap(_.inferences)
  }
  def theorems: Seq[Theorem] = {
    entries.ofType[Theorem]
  }
}
