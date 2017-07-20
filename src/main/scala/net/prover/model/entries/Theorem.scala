package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.{Premise, RearrangementType}
import net.prover.model.{Proof, EntryInference, Inference, ProofOutline, ProvenStatement}

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
    proof: Proof,
    rearrangementType: RearrangementType,
    allowsRearrangement: Boolean = true)
  extends ChapterEntry(TheoremOutline)
    with EntryInference
{
  def referencedInferenceIds: Set[String] = proof.referencedInferenceIds
  override def inferences: Seq[Inference] = Seq(this)
}
