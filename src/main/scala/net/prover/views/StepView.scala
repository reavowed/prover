package net.prover.views

import net.prover.JsonMapping
import net.prover.model.{DisplayContext, HtmlHelper, Inference}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{InferenceApplication, PreviousLineReference, Step}
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
      <a href={inference.key.url}>
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

  private def popoverForTarget(): Popover = {
    Popover(
      <span>Target</span>,
      <div>
        <button type="button"
                class="btn btn-success proveStatement"
                data-toggle="modal"
                data-target="#proveStatementModal"
        >
          Prove
        </button>
      </div>)
  }

  private def AssertionPremise(premise: NewAssert.Premise, path: Seq[Int])(implicit displayContext: DisplayContext): NodeSeq = {
    <div class="assertionPremise">
      {premise match {
        case NewAssert.Premise.Simplification(statement, _, _, _, _) =>
          <div>{ExpressionView(statement)}</div>
        case NewAssert.Premise.Pending(statement) =>
          <div class="editablePremise" data-path={path.mkString(".")}>{ExpressionView(statement)}</div>
        case NewAssert.Premise.Given(statement, _) =>
          <div>{ExpressionView(statement)}</div>
        case NewAssert.Premise.Expansion(statement, _, premises, _) =>
          Seq(
            <div>{ExpressionView(statement)}</div>,
            <div class="proofIndent">{premises.mapWithIndex((p, i) => AssertionPremise(p, path :+ i))}</div>)
      }}
    </div>
  }

  private def popoverForNewAssert(inference: Inference, premises: Seq[NewAssert.Premise])(implicit displayContext: DisplayContext): Popover = {
    Popover(
      <a href={inference.key.url}>
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
        <strong>Premises</strong>
        {premises.mapWithIndex((p, index) => AssertionPremise(p, Seq(index)))}
      </div>)

  }

  private case class Popover(title: Elem, content: Elem)

  private def lineView(
    prefix: String,
    statement: Statement,
    reference: String,
    premiseReferences: Set[PreviousLineReference],
    additionalAttributes: Map[String, String],
    additionalClasses: Seq[String],
    popover: Option[Popover],
    children: Option[Elem])(
    implicit displayContext: DisplayContext
  ): Elem = {
    val lineElement =
      <span class={("proofLine" +: additionalClasses).mkString(" ")}
            data-reference={reference}
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

  def apply(step: Step, path: Seq[Int])(implicit displayContext: DisplayContext): NodeSeq = {
    step match {
      case Step.Assertion(statement, inferenceApplication, _) =>
        lineView(
          "Then",
          statement,
          path.mkString("."),
          inferenceApplication.referencedLines,
          Map.empty,
          Nil,
          Some(popover(inferenceApplication)),
          None)
      case Step.Deduction(assumption, substeps, _, _) =>
        lineView(
          "Assume",
          assumption,
          path.mkString(".") + "a",
          Set.empty,
          Map.empty,
          Nil,
          None,
          Some(<div class="children proofIndent">{substeps.flatMapWithIndex((s, i) => StepView(s, path :+ i))}</div>))
      case Step.Naming(variableName, assumption, substeps, finalInferenceApplication, _) =>
        val innerContext = displayContext.withBoundVariableList(Seq(variableName))
        lineView(
          s"Let $variableName be such that",
          assumption,
          path.mkString(".") + "a",
          finalInferenceApplication.referencedLines,
          Map("data-reference-for-last-child" -> path.mkString(".")),
          Nil,
          Some(popover(finalInferenceApplication)),
          Some(<div class="children">{substeps.flatMapWithIndex((s, i) => StepView(s, path :+ i)(innerContext))}</div>))(
          innerContext)
      case Step.ScopedVariable(variableName, substeps, _, _) =>
        val innerContext = displayContext.withBoundVariableList(Seq(variableName))
        substeps.flatMapWithIndex((s, i) => StepView(s, path :+ i)(innerContext))
      case Step.Target(statement, _) =>
        Seq(lineView(
          "Then",
          statement,
          path.mkString("."),
          Set.empty,
          Map.empty,
          Seq("highlightIncomplete"),
          Some(popoverForTarget()),
          None))
      case step @ Step.NewAssert(statement, inference, premises, _, _) =>
        Seq(lineView(
          "Then",
          statement,
          path.mkString("."),
          step.referencedLines,
          Map("data-editable" -> "true"),
          if (step.isIncomplete) Seq("highlightIncomplete") else Nil,
          Some(popoverForNewAssert(inference, premises)),
          None))
    }
  }
}
