package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.Statement
import net.prover.model.{Inference, Premise}

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
  extends ChapterEntry
    with Inference.Entry
{
  override def inferences: Seq[Inference] = Seq(this)
}
