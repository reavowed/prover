package net.prover.views

import net.prover.model.{Book, Chapter, DisplayContext, HtmlHelper}
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.proof.ProofLine.Justification
import net.prover.model.proof.ReferenceMap

object TheoremView {
  private def popoverTitle(justification: Justification): String = {
    justification.inference match {
      case Some(inference) =>
        <a href={inference.entryKey.url}>{justification.text}</a>.toString()
      case None =>
        justification.text
    }
  }
  private def popoverContent(justification: Justification)(implicit displayContext: DisplayContext): String = {
    justification.inference match {
      case Some(inference) =>
        <div>
          { PremisesView(inference.premises, ReferenceMap.empty) }
          <div>
            { if(inference.premises.nonEmpty) "Then" }
            {ExpressionView(inference.conclusion)}.
          </div>
        </div>.toString()
      case None =>
        ""
    }
  }

  def apply(
    theorem: Theorem,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry.WithKey],
    nextOption: Option[ChapterEntry.WithKey],
    usages: Seq[(Book, Chapter, Seq[Theorem])]
  ) = InferenceView("Theorem", theorem, chapter, book, previousOption, nextOption, theorem.proof.referenceMap, usages) {
    import book.displayContext
    <div>
      <div class="theoremProof">
        <hr/>
        <h4>Proof</h4>
        <div class="proof">
          {theorem.proof.getLines.map { line =>
            <div class="proofLine" data-reference={line.reference.orNull}>
              <span class="popover-holder"
                    title={line.justification.map(popoverTitle).orNull}
                    data-content={line.justification.map(popoverContent).orNull}>
                { for(_ <- 1 to line.indentLevel) yield { <span>&nbsp;&nbsp;</span> } }
                {HtmlHelper.format(line.prefix)}
                <span class={line.reference.map("conclusion-" + _).orNull}>{ExpressionView(line.expression)}</span>.
              </span>
            </div>
          }}
        </div>
      </div>
      <script src="/js/theorem.js"></script>
    </div>
  }
}
