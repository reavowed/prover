package net.prover.model.proof

import net.prover.model._

case class InferenceApplication(
    inferenceSummary: Inference.Summary,
    substitutions: Inference.Substitutions,
    references: Seq[Reference])
{
  def referencedInferenceIds: Set[String] = references.flatMap(_.referencedInferenceIds).toSet + inferenceSummary.id
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
    AssertionHint.attempt(inferenceSummary, availableInferences, substitutions).toSeq ++
      references.flatMap(_.getAssertionHints(availableInferences))
  }
  def serializedLines: Seq[String] = Seq(inferenceSummary.serialized + " " + substitutions.serialized + " {") ++
    references.flatMap(_.serializedLines).indent :+
    "}"
}

object InferenceApplication {

  def parser(implicit parsingContext: ParsingContext): Parser[InferenceApplication] = {
    for {
      inferenceSummary <- Inference.Summary.parser
      substitutions <- Inference.Substitutions.parser
      references <- Reference.listParser.inBraces
    } yield InferenceApplication(inferenceSummary, substitutions, references)
  }

}
