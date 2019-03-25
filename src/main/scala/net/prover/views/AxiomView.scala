package net.prover.views

import net.prover.model.{Book, Chapter}
import net.prover.model.entries.{Axiom, ChapterEntry, Theorem}

object AxiomView {
  def apply(
    axiom: Axiom,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry],
    nextOption: Option[ChapterEntry],
    usages: Seq[(Book, Chapter, Seq[Theorem])]
  ) = InferenceView("Axiom", axiom, chapter, book, previousOption, nextOption, usages) { <div></div> }
}
