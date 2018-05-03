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
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement)
  extends ChapterEntry
    with Inference.Entry
{
  override def inferences: Seq[Inference] = Seq(this)
  override def serializedLines: Seq[String] = {
    Seq(s"axiom $name") ++
      rearrangementType.serialized.toSeq ++
      premises.map(_.serialized) ++
      Seq(s"conclusion ${conclusion.serialized}")
  }
}
