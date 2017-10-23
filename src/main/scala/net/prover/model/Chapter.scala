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
  def termDefinitions: Seq[TermDefinition] = {
    entries.ofType[TermDefinition]
  }

  def inferences: Seq[Inference] = {
    entries.flatMap(_.inferences)
  }
  def theorems: Seq[Theorem] = {
    entries.ofType[Theorem]
  }

  def expandOutlines(
    previousProofEntries: ProofEntries,
    cachedProofs: Seq[CachedProof]
  ): Chapter = {
    copy(entries = entries.mapFold[ChapterEntry] { case (entry, previousEntries) =>
      def inferencesSoFar = previousEntries.flatMap(_.inferences)
      def statementDefinitionsSoFar = previousEntries.ofType[StatementDefinition]
      def nextInferenceKey(name: String): String = {
        inferencesSoFar.count(_.name == name) match {
          case 0 =>
            name.formatAsKey
          case n =>
            (name + " " + (n+1)).formatAsKey
        }
      }
      entry match {
        case axiomOutline: AxiomOutline =>
          axiomOutline.expand(nextInferenceKey(axiomOutline.name), title, bookTitle)
        case theoremOutline: TheoremOutline =>
          theoremOutline.prove(
            nextInferenceKey(theoremOutline.name),
            title,
            bookTitle,
            previousProofEntries ++ ProofEntries(inferencesSoFar, statementDefinitionsSoFar),
            cachedProofs)
        case other =>
          other
      }
    })
  }
}
