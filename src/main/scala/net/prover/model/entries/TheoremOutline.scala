package net.prover.model.entries

import net.prover.model.Inference.{Premise, RearrangementType}
import net.prover.model._

case class TheoremOutline(
    name: String,
    premises: Seq[Premise],
    proofOutline: ProofOutline,
    rearrangementType: RearrangementType,
    allowsRearrangement: Boolean = true)
  extends ChapterEntry(TheoremOutline)
{
  def prove(
    availableInferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform],
    theoremCache: Seq[Theorem],
    chapterTitle: String,
    bookTitle: String,
    nextInferenceKey: String => String
  ): Theorem = {
    val detailedProof = getProofFromCache(theoremCache, availableInferences) getOrElse prove(availableInferences, inferenceTransforms, bookTitle)
    Theorem(
      name,
      nextInferenceKey(name),
      chapterTitle.formatAsKey,
      chapterTitle,
      bookTitle.formatAsKey,
      bookTitle,
      premises,
      detailedProof.conclusion,
      proofOutline,
      detailedProof,
      rearrangementType,
      allowsRearrangement)
  }

  private def getProofFromCache(
    theoremCache: Seq[Theorem],
    availableInferences: Seq[Inference]
  ): Option[DetailedProof] = {
    theoremCache
      .find { cachedTheorem =>
        cachedTheorem.premises == premises &&
          cachedTheorem.proofOutline == proofOutline &&
          cachedTheorem.referencedInferenceIds.forall(id => availableInferences.exists(_.id == id))
      }
      .map(_.proof)
  }

  private def prove(
    availableInferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform],
    bookName: String
  ): DetailedProof = {
    DetailedProof.fillInOutline(premises, proofOutline, availableInferences, inferenceTransforms, bookName, name)
  }
}

object TheoremOutline  extends ChapterEntryParser[TheoremOutline] with InferenceParser {
  override val name: String = "theorem"

  override def parser(implicit context: ParsingContext): Parser[TheoremOutline] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      allowsRearrangement <- Parser.optionalWord("disallow-rearrangement").isUndefined
      premises <- premisesParser
      proofOutline <- ProofOutline.parser
      _ <- Parser.requiredWord("qed")
    } yield {
      TheoremOutline(
        name,
        premises,
        proofOutline,
        rearrangementType,
        allowsRearrangement)
    }
  }
}
