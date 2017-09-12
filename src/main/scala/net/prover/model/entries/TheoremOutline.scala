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
    transformations: Seq[StatementDefinition],
    cachedProofs: Seq[CachedProof]
  ): Theorem = {
    val detailedProof = getProof(cachedProofs, availableInferences, transformations, key, bookTitle)
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
    transformations: Seq[StatementDefinition],
    key: String,
    bookTitle: String
  ): Proof = {
    cachedProofs
      .find { cachedProof =>
        premises == cachedProof.premises && cachedProof.matchesOutline(proofOutline)
      } match {
        case Some(cachedProof) =>
          cachedProof.validate(availableInferences, transformations) match {
            case Some(validProof) =>
              validProof
            case None =>
              TheoremOutline.logger.info(s"Cached proof for theorem $key was invalid - reproving")
              prove(availableInferences, cachedProof.getAssertionHints(availableInferences), transformations, bookTitle)
          }
        case None =>
          TheoremOutline.logger.info(s"No cached proof for theorem $key - proving directly")
          val assertionHints = cachedProofs.filter(_.premises == premises).flatMap(_.getAssertionHints(availableInferences))
          prove(availableInferences, assertionHints, transformations, bookTitle)
      }
  }

  private def prove(
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint],
    transformations: Seq[StatementDefinition],
    bookName: String
  ): Proof = {
    try {
      Proof.fillInOutline(premises, proofOutline, availableInferences, assertionHints, transformations)
    } catch {
      case NonFatal(e) =>
        throw new Exception(s"Error proving theorem $name in book $bookName", e)
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
