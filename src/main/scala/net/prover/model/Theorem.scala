package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

case class Theorem(
    name: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    proof: DetailedProof)
  extends ChapterEntry(Theorem)
    with Inference
{
  val id = calculateHash()
}

object Theorem extends ChapterEntryParser[Theorem] with InferenceParser {
  override val name: String = "theorem"

  override def parser(implicit context: Context): Parser[Theorem] = {
    for {
      name <- Parser.toEndOfLine
      premises <- premisesParser
      proofOutline <- ProofOutline.parser
      _ <- Parser.singleWord.matchOrThrow(_ == "qed", word => s"Expected step or qed, found '$word'")
    } yield {
      val detailedProof = DetailedProof.fillInOutline(premises, proofOutline)
      val conclusion = detailedProof.steps.ofType[DetailedProof.StepWithProvenStatement].lastOption
        .getOrElse(throw new Exception("Theorem must contain at least one top-level proven statement"))
        .provenStatement
      Theorem(
        name,
        premises,
        conclusion,
        detailedProof)
    }
  }

  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(inferences = context.inferences :+ theorem)
  }
}
