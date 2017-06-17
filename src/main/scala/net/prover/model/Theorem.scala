package net.prover.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.{Premise, RearrangementType}

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement", "proofOutline"))
case class Theorem(
    name: String,
    key: String,
    chapterKey: String,
    chapterTitle: String,
    bookKey: String,
    bookTitle: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    proofOutline: ProofOutline,
    proof: DetailedProof,
    rearrangementType: RearrangementType,
    allowsRearrangement: Boolean = true)
  extends ChapterEntry(Theorem)
    with Inference
{
  val id = calculateHash()
  def keyOption = Some(key)
  def referencedInferenceIds: Set[String] = proof.referencedInferenceIds
}

object Theorem extends ChapterEntryParser[Theorem] with InferenceParser {
  override val name: String = "theorem"

  override def parser(book: Book, chapter: Chapter)(implicit context: Context): Parser[Theorem] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      allowsRearrangement <- Parser.optionalWord("disallow-rearrangement").isUndefined
      premises <- premisesParser
      proofOutline <- ProofOutline.parser
      _ <- Parser.requiredWord("qed")
    } yield {
      val detailedProof = getProofFromCache(premises, proofOutline) getOrElse prove(premises, proofOutline)
      val conclusion = detailedProof.steps.ofType[DetailedProof.StepWithProvenStatement].lastOption
        .getOrElse(throw new Exception("Theorem must contain at least one top-level proven statement"))
        .provenStatement
      Theorem(
        name,
        context.nextInferenceKey(name),
        chapter.key,
        chapter.title,
        book.key,
        book.title,
        premises,
        conclusion,
        proofOutline,
        detailedProof,
        rearrangementType,
        allowsRearrangement)
    }
  }

  private def getProofFromCache(
    premises: Seq[Premise],
    proofOutline: ProofOutline)(
    implicit context: Context
  ): Option[DetailedProof] = {
    val id = Inference.calculateHash(premises, proofOutline.steps.ofType[ProofOutline.StepWithAssertion].last.assertion)
    context.theoremCache.get(id)
      .filter { cachedTheorem =>
        cachedTheorem.proofOutline == proofOutline &&
        cachedTheorem.referencedInferenceIds.forall(id => context.inferences.exists(_.id == id))
      }
      .map(_.proof)
  }

  private def prove(
    premises: Seq[Premise],
    proofOutline: ProofOutline)(
    implicit context: Context
  ): DetailedProof = {
    DetailedProof.fillInOutline(premises, proofOutline)
  }

  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(inferences = context.inferences :+ theorem)
  }
}
