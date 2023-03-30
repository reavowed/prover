package net.prover.entries

import net.prover.books.model.Book
import net.prover.model.definitions.Definitions
import net.prover.model.entries.Theorem.Proof
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.{AvailableEntries, ExpressionParsingContext, Inference, ProvingContext}

case class TypedEntryWithContext[+T <: ChapterEntry](
    entry: T,
    entryKey: String,
    chapterWithContext: ChapterWithContext
) {
  def allBooks: List[Book] = chapterWithContext.allBooks
  def definitions: Definitions = chapterWithContext.definitions
  def globalContext: GlobalContext = chapterWithContext.globalContext
  def book: Book = chapterWithContext.book
  def bookKey: String = chapterWithContext.bookKey
  def chapter = chapterWithContext.chapter
  def chapterKey = chapterWithContext.chapterKey

  def theorem(implicit ev: <:<[T, Theorem]): Theorem = ev.apply(entry)
  def inference(implicit ev: <:<[T, Inference]): Inference = ev.apply(entry)
  implicit def expressionParsingContext(implicit ev: <:<[T, Inference]): ExpressionParsingContext = ExpressionParsingContext.forInference(ev.apply(entry))
  def proofsWithContext(implicit ev: <:<[T, Theorem]): Seq[ProofWithContext] = ev.apply(entry).proofs.mapWithIndex((proof, i) => ProofWithContext(proof, i, this.asInstanceOf[TheoremWithContext]))
  def atProof(proof: Proof)(implicit ev: <:<[T, Theorem]): ProofWithContext = ProofWithContext(proof, ev.apply(entry).proofs.indexOf(proof), this.asInstanceOf[TheoremWithContext])

  lazy implicit val availableEntries: AvailableEntries = AvailableEntries.forEntry(this)
  lazy implicit val provingContext: ProvingContext = ProvingContext(availableEntries, chapterWithContext.bookWithContext.globalContext.definitions)
}
