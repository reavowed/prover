package net.prover.model

import net.prover.model.entries._
import net.prover.model.proof.{CachedProof, ProofEntries}

case class ChapterOutline(
    title: String,
    summary: String,
    bookTitle: String,
    entryOutlines: Seq[ChapterEntryOutline] = Nil)
{
  val key: String = title.formatAsKey
  val bookKey: String = bookTitle.formatAsKey

  def statementDefinitions: Seq[StatementDefinition] = {
    entryOutlines.ofType[StatementDefinition]
  }
  def termDefinitions: Seq[TermDefinition] = {
    entryOutlines.ofType[TermDefinition]
  }

  def expand(
    previousProofEntries: ProofEntries,
    cachedProofs: Seq[CachedProof]
  ): Chapter = {
    val entries = entryOutlines.mapFold[ChapterEntry] { case (previousEntries, entry) =>
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
        case selfOutline: ChapterEntry.SelfOutline =>
          selfOutline
      }
    }
    Chapter(title, summary, bookTitle, entries)
  }
}