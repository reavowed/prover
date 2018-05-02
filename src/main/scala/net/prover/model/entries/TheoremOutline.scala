package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.proof._
import org.slf4j.LoggerFactory

import scala.util.control.NonFatal

case class TheoremOutline(
    name: String,
    premises: Seq[Premise],
    proofOutline: ProofOutline,
    rearrangementType: RearrangementType)
  extends ChapterEntryOutline
{
  def prove(
    key: String,
    chapterTitle: String,
    bookTitle: String,
    proofEntries: ProofEntries,
    cachedProofs: Seq[CachedProof]
  ): Theorem = {
    val detailedProof = getProof(cachedProofs, proofEntries, key, bookTitle)
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
      rearrangementType)
  }

  private def getProof(
    cachedProofs: Seq[CachedProof],
    proofEntries: ProofEntries,
    key: String,
    bookTitle: String
  ): Proof = {
    cachedProofs
      .find { cachedProof =>
        premises == cachedProof.premises && cachedProof.matchesOutline(proofOutline)
      } match {
        case Some(cachedProof) =>
          cachedProof.validate(proofEntries) match {
            case Some(validProof) =>
              validProof
            case None =>
              TheoremOutline.logger.info(s"Cached proof for theorem $key was invalid - reproving")
              prove(cachedProof.getAssertionHints(proofEntries.availableInferences), proofEntries, bookTitle)
          }
        case None =>
          TheoremOutline.logger.info(s"No cached proof for theorem $key - proving directly")
          val assertionHints = cachedProofs.filter(_.premises == premises)
            .flatMap(_.getAssertionHints(proofEntries.availableInferences))
          prove(assertionHints, proofEntries, bookTitle)
      }
  }

  private def prove(
    assertionHints: Seq[CachedStep.Assertion],
    proofEntries: ProofEntries,
    bookName: String
  ): Proof = {
    try {
      proofOutline.fillIn(ProvingContext.getInitial(premises, assertionHints, proofEntries))
    } catch {
      case NonFatal(e) =>
        throw new Exception(s"Error proving theorem $name in book $bookName\n${e.getMessage}")
    }
  }
}

object TheoremOutline extends ChapterEntryParser {
  val logger = LoggerFactory.getLogger(TheoremOutline.getClass)
  override val name: String = "theorem"

  override def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[TheoremOutline] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- Premise.listParser
      proofOutline <- ProofOutline.parser
      _ <- Parser.requiredWord("qed")
    } yield {
      TheoremOutline(
        name,
        premises,
        proofOutline,
        rearrangementType)
    }
  }
}
