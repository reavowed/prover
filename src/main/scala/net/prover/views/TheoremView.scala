package net.prover.views

import net.prover.model.{Book, Chapter, HtmlHelper}
import net.prover.model.entries.{ChapterEntry, Theorem}

import scala.xml.{Text, Unparsed}

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
      <script type="text/javascript">
        {Unparsed("""$(function() {
          $("[data-reference]").each(function() {
            var $this = $(this);
            var escapedReference = _.replace($this.attr("data-reference"), /\./g, "\\.");
            var $premises = $(".highlight-" + escapedReference);
            var $conclusion = $this.find('.conclusion-' + escapedReference);
            $(this)
              .on("mouseenter", function() {
                $premises.addClass("highlightPremise");
                $conclusion.addClass("highlightConclusion");
              })
              .on("mouseleave", function() {
                $premises.removeClass("highlightPremise");
                $conclusion.removeClass("highlightConclusion");
              })
          })
        })""")}
      </script>
    </div>
  }
}
