package net.prover.views

import net.prover.JsonMapping
import net.prover.model.{DisplayContext, HtmlHelper, Inference}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{InferenceApplication, PreviousLineReference, Reference, Step}
import net.prover.model.proof.Step.NewAssert

import scala.xml._

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

  private def AssertionPremise(premise: NewAssert.Premise, path: Seq[Int])(implicit displayContext: DisplayContext): NodeSeq = {
    <div class="assertionPremise">
      {premise match {
        case NewAssert.Premise.Pending(statement) =>
          <div class="editablePremise" data-path={path.mkString(".")}>{ExpressionView(statement)}</div>
        case NewAssert.Premise.Given(statement, _) =>
          <div>{ExpressionView(statement)}</div>
        case NewAssert.Premise.Rearrangement(statement, _, premises, _) =>
          Seq(
            <div>{ExpressionView(statement)}</div>,
            <div class="proofIndent">{premises.mapWithIndex((p, i) => AssertionPremise(p, path :+ i))}</div>)
      }}
    </div>
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
        {premises.mapWithIndex((p, index) => AssertionPremise(p, Seq(index)))}
      </div>)

  }

  private case class Popover(title: Elem, content: Elem)

  private def lineView(
    prefix: String,
    statement: Statement,
    reference: Reference.Direct,
    premiseReferences: Set[PreviousLineReference],
    additionalAttributes: Map[String, String],
    popover: Option[Popover],
    children: Option[Elem])(
    implicit displayContext: DisplayContext
  ): Elem = {
    val lineElement =
      <span class="proofLine"
            data-reference={reference.value}
            data-premise-references={JsonMapping.toString(premiseReferences)}
            data-title={popover.map(_.title.toString()).orNull}
            data-content={popover.map(_.content.toString()).orNull}>
        {HtmlHelper.format(prefix)}
        <span class="conclusion">{ExpressionView(statement)}</span>.
      </span>
    val newAttributes = additionalAttributes.foldRight[MetaData](Null) { case ((key, value), currentAttributes) =>
      new UnprefixedAttribute(key, Text(value), currentAttributes)
    }
    // Because MetaData.append actually PREpends, and we're fussy
    def append(head: MetaData, tail: MetaData): MetaData = head match {
      case Null =>
        tail
      case attribute: Attribute =>
        attribute.copy(append(attribute.next, tail))
    }

    <div class="proofStep">
      {lineElement.copy(attributes = append(lineElement.attributes, newAttributes))}
      {children.orNull}
    </div>
  }

  def apply(step: Step)(implicit displayContext: DisplayContext): NodeSeq = {
    step match {
      case Step.Assertion(statement, inferenceApplication, reference) =>
        lineView(
          "Then",
          statement,
          reference,
          inferenceApplication.referencedLines,
          Map.empty,
          Some(popover(inferenceApplication)),
          None)
      case Step.Assumption(assumption, substeps, _, reference) =>
        lineView(
          "Assume",
          assumption,
          reference.getChildForAssumption,
          Set.empty,
          Map.empty,
          None,
          Some(<div class="children proofIndent">{substeps.flatMap(StepView(_))}</div>))
      case Step.Naming(variableName, assumption, substeps, finalInferenceApplication, reference) =>
        val innerContext = displayContext.withBoundVariableList(Seq(variableName))
        lineView(
          s"Let $variableName be such that",
          assumption,
          reference.getChildForAssumption,
          finalInferenceApplication.referencedLines,
          Map("data-reference-for-last-child" -> reference.value),
          Some(popover(finalInferenceApplication)),
          Some(<div class="children">{substeps.flatMap(StepView(_)(innerContext))}</div>))(
          innerContext)
      case Step.ScopedVariable(variableName, substeps, _, _) =>
        val innerContext = displayContext.withBoundVariableList(Seq(variableName))
        substeps.flatMap(StepView(_)(innerContext))
      case Step.Target(statement, reference, _) =>
        Seq(lineView(
          "Target:",
          statement,
          reference,
          Set.empty,
          Map.empty,
          Some(popoverForTarget(reference)),
          None))
      case Step.NewAssert(statement, inference, premises, _, reference, _) =>
        Seq(lineView(
          "Then",
          statement,
          reference,
          Set.empty,
          Map("data-editable" -> "true"),
          Some(popoverForNewAssert(inference, premises)),
          None))
    }
  }
}
