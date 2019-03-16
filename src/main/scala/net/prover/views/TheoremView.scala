package net.prover.views

import net.prover.model._
import net.prover.model.entries.{ChapterEntry, Theorem}
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step.NewAssert
import net.prover.model.proof._

import scala.xml.Elem

object TheoremView {
  private def popover(inferenceApplication: InferenceApplication)(implicit displayContext: DisplayContext): Popover = {
    if (inferenceApplication.isRearrangement) {
      Popover(<span>Rearrangement</span>, <span/>)
    } else {
      val inference = HtmlHelper.findInferenceToDisplay(inferenceApplication)
      popoverForInference(inference)
    }
  }

  private def popoverForInference(inference: Inference)(implicit displayContext: DisplayContext): Popover = {
    Popover(
      <a href={inference.entryKey.url}>
        {inference.name}
      </a>,
      <div>
        {PremisesView(inference.premises, ReferenceMap.empty)}
        <div>
        {if (inference.premises.nonEmpty) "Then" }
        {ExpressionView(inference.conclusion)}.
        </div>
      </div>)
  }

  private def popoverForTarget(reference: Reference.Direct): Popover = {
    Popover(
      <span>"Statement to be proved"</span>,
      <div>
        <button type="button"
                class="btn btn-success proveStatement"
                data-toggle="modal"
                data-target="#proveStatementModal"
                data-reference={reference.value}
        >
          Prove
        </button>
      </div>)
  }

  private def popoverForNewAssert(inference: Inference, premises: Seq[NewAssert.Premise])(implicit displayContext: DisplayContext): Popover = {
    Popover(
      <a href={inference.entryKey.url}>
        {inference.name}
      </a>,
      <div>
        <div>
          {PremisesView(inference.premises, ReferenceMap.empty)}
          <div>
            {if (inference.premises.nonEmpty) "Then" }
            {ExpressionView(inference.conclusion)}.
          </div>
        </div>
        <hr />
        <h5>Premises</h5>
        {premises.map { p => <div>{ExpressionView(p.statement)}</div>}}
      </div>)

  }

  private case class Popover(title: Elem, content: Elem)

  private def lineView(
    prefix: String,
    statement: Statement,
    indentLevel: Int,
    reference: Reference.Direct,
    additionalReference: Option[String],
    popover: Option[Popover],
    referenceMap: ReferenceMap)(
    implicit displayContext: DisplayContext
  ): Elem = {
    <div class="proofLine" data-reference={reference.value}>
      <span class="popover-holder"
            data-title={popover.map(_.title.toString()).orNull}
            data-content={popover.map(_.content.toString()).orNull}>
        { for(_ <- 1 to indentLevel) yield { <span>&nbsp;&nbsp;</span> } }
        {HtmlHelper.format(prefix)}
        <span class={"conclusion-" + reference.value}>{ExpressionView(statement, referenceMap.getReferrers(reference.value, additionalReference))}</span>.
      </span>
    </div>
  }

  private def stepView(step: Step, indentLevel: Int, additionalReference: Option[String], referenceMap: ReferenceMap)(implicit displayContext: DisplayContext): Seq[Elem] = {
    step match {
      case Step.Assertion(statement, inferenceApplication, reference) =>
        Seq(lineView(
          "Then",
          statement,
          indentLevel,
          reference,
          additionalReference,
          Some(popover(inferenceApplication)),
          referenceMap))
      case Step.Assumption(assumption, substeps, _, reference) =>
        val assumptionLine = lineView(
          "Assume",
          assumption,
          indentLevel,
          reference.getChildForAssumption,
          additionalReference,
          None,
          referenceMap)
        val substepLines = substeps.flatMapWithIndex { (substep, index) =>
          stepView(substep, indentLevel + 1, if (index == substeps.length - 1) additionalReference else None, referenceMap)
        }
        assumptionLine +: substepLines
      case Step.Naming(variableName, assumption, substeps, _, reference) =>
        val firstLine = lineView(
          s"Let $variableName be such that",
          assumption,
          indentLevel,
          reference.getChildForAssumption,
          None,
          None,
          referenceMap)
        val substepLines = substeps.flatMapWithIndex { (substep, index) =>
          stepView(substep, indentLevel, if (index == substeps.length - 1) Some(additionalReference.getOrElse(reference.value)) else None, referenceMap)
        }
        firstLine +: substepLines
      case Step.ScopedVariable(_, substeps, _, _) =>
        substeps.flatMapWithIndex { (substep, index) =>
          stepView(substep, indentLevel, if (index == substeps.length - 1) additionalReference else None, referenceMap)
        }
      case Step.Target(statement, reference) =>
        Seq(lineView(
          "Target:",
          statement,
          indentLevel,
          reference,
          additionalReference,
          Some(popoverForTarget(reference)),
          referenceMap))
      case Step.NewAssert(statement, inference, premises, _, reference) =>
        Seq(lineView(
          "Then",
          statement,
          indentLevel,
          reference,
          additionalReference,
          Some(popoverForNewAssert(inference, premises)),
          referenceMap))
    }
  }

  def apply(
    theorem: Theorem,
    chapter: Chapter,
    book: Book,
    previousOption: Option[ChapterEntry.WithKey],
    nextOption: Option[ChapterEntry.WithKey],
    usages: Seq[(Book, Chapter, Seq[Theorem])]
  ): Elem = InferenceView("Theorem", theorem, chapter, book, previousOption, nextOption, theorem.proof.referenceMap, usages) {
    import book.displayContext
    val referenceMap = theorem.proof.referenceMap
    <div>
      <div class="theoremProof">
        <hr/>
        <h4>Proof</h4>
        <div class="proof">
          {theorem.proof.steps.flatMap(stepView(_, 0, None, referenceMap))}
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
