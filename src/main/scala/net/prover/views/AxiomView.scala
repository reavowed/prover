package net.prover.views

import net.prover.model.{Book, Chapter}
import net.prover.model.entries.{Axiom, ChapterEntry, Theorem}
import net.prover.model.proof.ReferenceMap

object AxiomView {
  def apply(
    axiom: Axiom,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry.WithKey],
    nextOption: Option[ChapterEntry.WithKey],
    usages: Seq[(Book, Chapter, Seq[Theorem])]
  ) = InferenceView("Axiom", axiom, chapter, book, previousOption, nextOption, ReferenceMap.empty, usages) { <div></div> }
}
