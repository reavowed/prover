package net.prover.views

import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}

import scala.xml.Elem

object TheoremView {
  def apply(
    theorem: Theorem,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry.WithKey],
    nextOption: Option[ChapterEntry.WithKey],
    usages: Seq[(Book, Chapter, Seq[Theorem])]
  ): Elem = InferenceView("Theorem", theorem, chapter, book, previousOption, nextOption, usages) {
    import book.displayContext
    <div>
      <div class="theoremProof">
        <hr/>
        <h4>Proof</h4>
        <div class="proof">
          {theorem.proof.steps.flatMap(StepView(_, 0, None))}
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
      <div class="modal" tabindex="-1" role="dialog" id="editBoundVariableModal">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
              <h4 class="modal-title">Edit bound variable name</h4>
            </div>
            <div class="modal-body">
              <div class="form-group">
                <label for="shorthandInput">Name</label>
                <input type="text" class="form-control" id="boundVariableNameInput" />
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
              <button type="button" class="btn btn-primary" id="saveBoundVariableNameButton">Save</button>
            </div>
          </div>
        </div>
      </div>
      <script src="/js/common.js"></script>
      <script src="/js/theorem.js"></script>
    </div>
  }
}
