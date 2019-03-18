package net.prover.views

import net.prover.JsonMapping
import net.prover.model.{DisplayContext, HtmlHelper, Inference}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{InferenceApplication, PreviousLineReference, Reference, Step}
import net.prover.model.proof.Step.NewAssert

import scala.xml.Elem

object StepView {

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
        {PremisesView(inference.premises)}
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
          {PremisesView(inference.premises)}
          <div>
            {if (inference.premises.nonEmpty) "Then" }
            {ExpressionView(inference.conclusion)}.
          </div>
        </div>
        <hr />
        <h5>Premises</h5>
        {premises.mapWithIndex { (p, index) => <div class="editablePremise" data-index={index.toString}>{ExpressionView(p.statement)}</div>}}
      </div>)

  }

  private case class Popover(title: Elem, content: Elem)

  private def lineView(
    prefix: String,
    statement: Statement,
    indentLevel: Int,
    reference: Reference.Direct,
    additionalReference: Option[String],
    premiseReferences: Set[PreviousLineReference],
    popover: Option[Popover])(
    implicit displayContext: DisplayContext
  ): Elem = {
    <div class="proofLine" data-reference={reference.value} data-additional-reference={additionalReference.orNull} data-premise-references={JsonMapping.toString(premiseReferences)}>
      <span class="popover-holder"
            data-title={popover.map(_.title.toString()).orNull}
            data-content={popover.map(_.content.toString()).orNull}>
        { for(_ <- 1 to indentLevel) yield { <span>&nbsp;&nbsp;</span> } }
        {HtmlHelper.format(prefix)}
        <span class="conclusion">{ExpressionView(statement)}</span>.
      </span>
    </div>
  }

  def apply(step: Step, indentLevel: Int, additionalReference: Option[String])(implicit displayContext: DisplayContext): Seq[Elem] = {
    step match {
      case Step.Assertion(statement, inferenceApplication, reference) =>
        Seq(lineView(
          "Then",
          statement,
          indentLevel,
          reference,
          additionalReference,
          inferenceApplication.referencedLines,
          Some(popover(inferenceApplication))))
      case Step.Assumption(assumption, substeps, _, reference) =>
        val assumptionLine = lineView(
          "Assume",
          assumption,
          indentLevel,
          reference.getChildForAssumption,
          additionalReference,
          Set.empty,
          None)
        val substepLines = substeps.flatMapWithIndex { (substep, index) =>
          StepView(substep, indentLevel + 1, if (index == substeps.length - 1) additionalReference else None)
        }
        assumptionLine +: substepLines
      case Step.Naming(variableName, assumption, substeps, finalInferenceApplication, reference) =>
        val innerContext = displayContext.withBoundVariableList(Seq(variableName))
        val firstLine = lineView(
          s"Let $variableName be such that",
          assumption,
          indentLevel,
          reference.getChildForAssumption,
          None,
          finalInferenceApplication.referencedLines,
          Some(popover(finalInferenceApplication)))(
          innerContext)
        val substepLines = substeps.flatMapWithIndex { (substep, index) =>
          StepView(substep, indentLevel, if (index == substeps.length - 1) Some(additionalReference.getOrElse(reference.value)) else None)(innerContext)
        }
        firstLine +: substepLines
      case Step.ScopedVariable(variableName, substeps, _, _) =>
        val innerContext = displayContext.withBoundVariableList(Seq(variableName))
        substeps.flatMapWithIndex { (substep, index) =>
          StepView(substep, indentLevel, if (index == substeps.length - 1) additionalReference else None)(innerContext)
        }
      case Step.Target(statement, reference) =>
        Seq(lineView(
          "Target:",
          statement,
          indentLevel,
          reference,
          additionalReference,
          Set.empty,
          Some(popoverForTarget(reference))))
      case Step.NewAssert(statement, inference, premises, _, reference) =>
        Seq(lineView(
          "Then",
          statement,
          indentLevel,
          reference,
          additionalReference,
          Set.empty,
          Some(popoverForNewAssert(inference, premises))))
    }
  }
}
