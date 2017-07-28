package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.proof.{AssertionHint, CachedProof, Proof, ProofOutline}
import org.slf4j.LoggerFactory

import scala.util.control.NonFatal

case class TheoremOutline(
    name: String,
    premises: Seq[Premise],
    proofOutline: ProofOutline,
    rearrangementType: RearrangementType,
    allowsRearrangement: Boolean = true)
  extends ChapterEntry(TheoremOutline)
{
  def prove(
    key: String,
    chapterTitle: String,
    bookTitle: String,
    availableInferences: Seq[Inference],
    cachedProofs: Seq[CachedProof]
  ): Theorem = {
    val detailedProof = getProof(cachedProofs, availableInferences, key, bookTitle)
    Theorem(
      name,
      key,
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

  private def getProof(
    cachedProofs: Seq[CachedProof],
    availableInferences: Seq[Inference],
    key: String,
    bookTitle: String
  ): Proof = {
    cachedProofs
      .find { cachedProof =>
        premises.zipStrict(cachedProof.premises).exists(_.forall {
          case (p1, p2) => p1.matches(p2)
        }) && cachedProof.proof.matchesOutline(proofOutline)
      } match {
        case Some(cachedProof) =>
          cachedProof.validate(availableInferences) match {
            case Some(validProof) =>
              validProof
            case None =>
              TheoremOutline.logger.info(s"Cached proof for theorem $key was invalid - reproving")
              prove(availableInferences, cachedProof.proof.getAssertionHints(availableInferences), bookTitle)
          }
        case None =>
          TheoremOutline.logger.info(s"No cached proof for theorem $key - proving directly")
          val assertionHints = cachedProofs.filter(_.premises == premises).flatMap(_.proof.getAssertionHints(availableInferences))
          prove(availableInferences, assertionHints, bookTitle)
      }
  }

  private def prove(
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint],
    bookName: String
  ): Proof = {
    try {
      Proof.fillInOutline(premises, proofOutline, availableInferences, assertionHints)
    } catch {
      case NonFatal(e) =>
        throw new Exception(s"Error proving theorem $name in book $bookName\n${e.getMessage}")
    }
  }
}

object TheoremOutline extends ChapterEntryParser[TheoremOutline] {
  val logger = LoggerFactory.getLogger(TheoremOutline.getClass)
  override val name: String = "theorem"

  override def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[TheoremOutline] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      allowsRearrangement <- Parser.optionalWord("disallow-rearrangement").isUndefined
      premises <- Premise.listParser
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
