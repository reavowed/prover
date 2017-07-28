package net.prover.model.entries

import net.prover.model.Inference.{Premise, RearrangementType}
import net.prover.model.components.Statement
import net.prover.model.{EntryInference, Inference}

case class Axiom(
    name: String,
    key: String,
    chapterKey: String,
    chapterTitle: String,
    bookKey: String,
    bookTitle: String,
    premises: Seq[Premise],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement,
    allowsRearrangement: Boolean = true)
  extends ChapterEntry(AxiomOutline)
    with EntryInference
{
  override def inferences: Seq[Inference] = Seq(this)
}
