package net.prover.model

import net.prover.model.Inference.Premise

case class Theorem(
    name: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    proof: DetailedProof)
  extends ChapterEntry(Theorem)
    with Inference
{
  val id = calculateHash()
  def referencedInferenceIds: Set[String] = proof.referencedInferenceIds
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
      getFromCache(name, premises, proofOutline) getOrElse prove(name, premises, proofOutline)
    }
  }

  private def getFromCache(
    name: String,
    premises: Seq[Premise],
    proofOutline: ProofOutline)(
    implicit context: Context
  ): Option[Theorem] = {
    val id = Inference.calculateHash(premises, proofOutline.steps.ofType[ProofOutline.StepWithAssertion].last.assertion)
    context.theoremCache.get(id)
      .filter {
        _.referencedInferenceIds.forall(id => context.inferences.exists(_.id == id))
      }
  }

  private def prove(
    name: String,
    premises: Seq[Premise],
    proofOutline: ProofOutline)(
    implicit context: Context
  ): Theorem = {
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

  override def addToContext(theorem: Theorem, context: Context): Context = {
    context.copy(inferences = context.inferences :+ theorem)
  }
}
