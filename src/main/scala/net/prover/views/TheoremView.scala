package net.prover.views

import net.prover.model.{Book, Chapter, HtmlHelper}
import net.prover.model.entries.{ChapterEntry, Theorem}

object TheoremView {
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
        <table class="table-condensed proofRows">
          {theorem.proof.getLines.map { line =>
            <tr class="proofRow" data-reference={line.reference.orNull}>
              <td>
                { for(_ <- 1 to line.indentLevel) yield { <span>&nbsp;&nbsp;</span> } }
                {HtmlHelper.format(line.prefix)}
                <span class={line.reference.map("conclusion-" + _).orNull}>{ExpressionView(line.expression)}</span>.
              </td>
              { line.inferenceLink.toSeq.map { inferenceLink =>
                <td>
                  <a class="stepInferenceId" href={inferenceLink.key.map(_.url).orNull}>
                    {inferenceLink.name}
                  </a>
                </td>
              }}
            </tr>
          }}
        </table>
      </div>
      <script src="/js/theorem.js"></script>
    </div>
  }
}
