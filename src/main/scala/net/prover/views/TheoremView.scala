package net.prover.views

import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.proof.ProofLine.Justification
import net.prover.model.proof.{ProofLine, ReferenceMap}

object TheoremView {
  private def popoverTitle(line: ProofLine): Option[String] = {
    line.justification.map {
      case Justification(text, Some(inference)) =>
        <a href={inference.entryKey.url}>{text}</a>.toString()
      case Justification(text, None) =>
        text
    } orElse line.requiresProof.ifTrue("Statement to be proved")
  }
  private def popoverContent(line: ProofLine)(implicit displayContext: DisplayContext): Option[String] = {
    line.justification.flatMap(_.inference).map { inference =>
      <div>
        { PremisesView(inference.premises, ReferenceMap.empty) }
        <div>
          { if(inference.premises.nonEmpty) "Then" }
          {ExpressionView(inference.conclusion)}.
        </div>
      </div>.toString()
    } orElse line.requiresProof.ifTrue {
      <div>
        <button type="button"
                class="btn btn-success addNamingStep"
                data-reference={line.reference.orNull}
        >
          Add Naming Step
        </button>
        <button type="button"
                class="btn btn-success proveStatement"
                data-toggle="modal"
                data-target="#proveStatementModal"
                data-reference={line.reference.orNull}
        >
          Prove
        </button>
      </div>.toString()
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
                    title={popoverTitle(line).orNull}
                    data-content={popoverContent(line).orNull}>
                { for(_ <- 1 to line.indentLevel) yield { <span>&nbsp;&nbsp;</span> } }
                {HtmlHelper.format(line.prefix)}
                <span class={line.reference.map("conclusion-" + _).orNull}>{ExpressionView(line.expression)}</span>.
              </span>
            </div>
          }}
        </div>
      </div>
      <div class="modal" tabindex="-1" role="dialog" id="proveStatementModal">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
              <h4 class="modal-title">Add naming step</h4>
            </div>
            <div class="modal-body">
              <div class="form-group">
                <label for="inferenceName">Select premise</label>
                <input type="text" class="form-control" id="inferenceName" />
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
              <button type="button" class="btn btn-primary" id="proveStatementSubmitButton">Add</button>
            </div>
          </div>
        </div>
      </div>
      <div class="modal" tabindex="-1" role="dialog" id="addNamingStepModal">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
              <h4 class="modal-title">Add naming step</h4>
            </div>
            <div class="modal-body">
              <div class="form-group">
                <label for="addNamingStepPremiseSelect">Select inference</label>
                <select class="form-control" id="addNamingStepPremiseSelect"></select>
              </div>
              <div class="form-group">
                <label for="addNamingStepVariableNameInput">Choose variable name</label>
                <input type="text" class="form-control" id="addNamingStepVariableNameInput" />
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
            </div>
          </div>
        </div>
      </div>
      <script src="/js/common.js"></script>
      <script src="/js/theorem.js"></script>
    </div>
  }
}
