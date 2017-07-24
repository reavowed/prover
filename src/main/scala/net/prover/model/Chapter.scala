package net.prover.model

import net.prover.model.entries._
import net.prover.model.proof.CachedProof

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
  def inferenceTransforms: Seq[InferenceTransform] = {
    entries.ofType[InferenceTransform]
  }
  def theorems: Seq[Theorem] = {
    entries.ofType[Theorem]
  }

  def expandOutlines(
    previousInferences: Seq[Inference],
    previousInferenceTransforms: Seq[InferenceTransform],
    cachedProofs: Seq[CachedProof]
  ): Chapter = {
    copy(entries = entries.mapFold[ChapterEntry] { case (entry, previousEntries) =>
      def inferencesSoFar = previousEntries.flatMap(_.inferences)
      def inferenceTransformsSoFar = previousEntries.ofType[InferenceTransform]
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
            previousInferences ++ inferencesSoFar,
            previousInferenceTransforms ++ inferenceTransformsSoFar,
            cachedProofs)
        case other =>
          other
      }
    })
  }
}
