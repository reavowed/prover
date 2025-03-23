package net.prover.entries

import net.prover.model.*
import net.prover.model.entries.Theorem.Proof
import net.prover.model.entries.{ChapterEntry, Theorem}

case class TypedEntryWithContext[+T <: ChapterEntry](
    entry: T,
    entryKey: String,
    chapterWithContext: ChapterWithContext
) {
  def globalContext: GlobalContext = chapterWithContext.globalContext

  def theorem(implicit ev: <:<[T, Theorem]): Theorem = ev.apply(entry)
  def inference(implicit ev: <:<[T, Inference]): Inference = ev.apply(entry)
  implicit def expressionParsingContext(implicit ev: <:<[T, Inference]): ExpressionParsingContext = ExpressionParsingContext.forInference(ev.apply(entry))
  def proofsWithContext(implicit ev: <:<[T, Theorem]): Seq[ProofWithContext] = ev.apply(entry).proofs.mapWithIndex((proof, i) => ProofWithContext(proof, i, this.asInstanceOf[TheoremWithContext]))
  def atProof(proof: Proof)(implicit ev: <:<[T, Theorem]): ProofWithContext = ProofWithContext(proof, ev.apply(entry).proofs.indexOf(proof), this.asInstanceOf[TheoremWithContext])

  lazy implicit val availableEntries: AvailableEntries = AvailableEntries.forEntry(this)
  lazy implicit val provingContext: ProvingContext = ProvingContext(availableEntries, globalContext.definitions)
}
