package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, Summary}
import net.prover.model.ProofOutline.StepWithAssertion
import net.prover.model.components.{Statement, TermVariable}

case class DetailedProof(steps: Seq[DetailedProof.Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  val conclusion: ProvenStatement = {
    steps.ofType[DetailedProof.StepWithProvenStatement].lastOption
      .getOrElse(throw new Exception("Proof must contain at least one top-level proven statement"))
      .provenStatement
  }
}

object DetailedProof {
  sealed trait Step {
    def referencedInferenceIds: Set[String]
  }
  sealed trait StepWithProvenStatement extends Step {
    def provenStatement: ProvenStatement
  }
  object StepWithProvenStatement {
    def unapply(step: Step): Option[ProvenStatement] = step match {
      case stepWithProvenStatement: StepWithProvenStatement =>
        Some(stepWithProvenStatement.provenStatement)
      case _ =>
        None
    }
  }
  case class AssumptionStep(
      assumption: Statement,
      steps: Seq[Step])
    extends Step {
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  }
  case class NamingStep(
      variable: TermVariable,
      assumptionStep: AssumptionStep,
      assertionStep: StepWithProvenStatement)
    extends StepWithProvenStatement {
    override def provenStatement: ProvenStatement = assertionStep.provenStatement
    override def referencedInferenceIds: Set[String] = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
  }
  case class AssertionStep(
      provenStatement: ProvenStatement,
      inference: Inference.Summary,
      references: Seq[Reference])
    extends StepWithProvenStatement {
    override def referencedInferenceIds: Set[String] = inference.id.toSet
  }
  case class TransformedInferenceStep(
      provenStatement: ProvenStatement,
      inference: Inference.Summary,
      transformationProof: DetailedProof,
      references: Seq[Reference])
    extends StepWithProvenStatement {
    override def referencedInferenceIds: Set[String] = transformationProof.referencedInferenceIds ++ inference.id.toSet
  }

  case class Simplification(result: Statement, previous: Seq[Statement])

  case class Rearrangement(
    inference: Inference.Summary,
    substitutions: Substitutions,
    provenStatement: ProvenStatement,
    references: Seq[Reference])

  sealed trait Reference
  case class DirectReference(index: Int, html: String) extends Reference
  case class DeducedReference(antecedentIndex: Int, consequentIndex: Int) extends Reference
  case class SimplifiedReference(index: Int, html: String, simplification: Simplification) extends Reference
  case class ElidedReference(inference: Summary, substitutions: Substitutions, references: Seq[Reference]) extends Reference {
    val referenceType = "elided"
  }

  case class ReferencedAssertion(provenStatement: ProvenStatement, reference: DirectReference)
  case class ReferencedDeduction(assumption: Statement, deduction: ProvenStatement, reference: Reference)

  def fillInOutline(
    premises: Seq[Premise],
    proofOutline: ProofOutline,
    availableInferences: Seq[Inference],
    inferenceTransforms: Seq[InferenceTransform],
    bookName: String,
    theoremName: String
  ): DetailedProof = {
    val premiseAssertions = premises.zipWithIndex.collect {
      case (DirectPremise(premise), index) =>
        ReferencedAssertion(ProvenStatement.withNoConditions(premise), DirectReference(index, premise.html))
    }
    val premiseDeductions = premises.zipWithIndex.collect {
      case (premise @ DeducedPremise(assumption, conclusion), index) =>
        ReferencedDeduction(assumption, ProvenStatement.withNoConditions(conclusion), DirectReference(index, premise.html))
    }
    val context = ProvingContext(
      premiseAssertions,
      premiseDeductions,
      premises,
      Nil,
      availableInferences,
      inferenceTransforms,
      bookName,
      theoremName)
    val (detailedSteps, _) = proveSteps(
      proofOutline.steps,
      Nil,
      context,
      premises.length)
    DetailedProof(detailedSteps)
  }

  private def proveSteps(
    stepOutlines: Seq[ProofOutline.Step],
    accumulatedSteps: Seq[Step],
    context: ProvingContext,
    nextReference: Int
  ): (Seq[Step], ProvingContext) = {
    stepOutlines match {
      case Nil =>
        (accumulatedSteps, context)
      case stepOutline +: otherStepOutlines =>
        val (step, updatedContext) = proveStep(stepOutline, context, nextReference)
        proveSteps(
          otherStepOutlines,
          accumulatedSteps :+ step,
          updatedContext,
          nextReference + 1)
    }
  }

  private def proveStep(
    stepOutline: ProofOutline.Step,
    context: ProvingContext,
    nextReference: Int
  ): (Step, ProvingContext) = {
    stepOutline match {
      case ProofOutline.AssumptionStep(assumption, substepOutlines) =>
        proveAssumptionStep(assumption, substepOutlines, context, nextReference)
      case ProofOutline.NamingStep(variable, namingStatement, substepOutlines) =>
        val finalStepWithAssertion = substepOutlines match {
          case _ :+ (step: StepWithAssertion) =>
            step.innermostAssertionStep
          case _ =>
            throw new Exception("Naming step must end with an assertion")
        }
        val (assumptionStep, assumptionContext) = proveAssumptionStep(namingStatement, substepOutlines, context, nextReference)
        val (assertionStep, updatedContext) = proveAssertionStep(
          finalStepWithAssertion,
          assumptionContext,
          nextReference
        ).getOrElse(
          throw new Exception(Seq(
            s"Could not extract assertion ${finalStepWithAssertion.assertion} from naming step for $variable",
            s"${context.bookName} - ${context.theoremName}",
            s"${finalStepWithAssertion.location.fileName} line ${finalStepWithAssertion.location.lineNumber}"
          ).mkString("\n")))
        (NamingStep(variable, assumptionStep, assertionStep), updatedContext)
      case assertionStep: ProofOutline.AssertionStep =>
        proveAssertionStep(assertionStep, context, nextReference)
          .getOrElse(throw new Exception(
            Seq(
              s"Could not prove assertion ${assertionStep.assertion}",
              s"${context.bookName} - ${context.theoremName}",
              s"${assertionStep.location.fileName} line ${assertionStep.location.lineNumber}"
            ).mkString("\n")))
    }
  }

  private def proveAssumptionStep(
    assumption: Statement,
    substepOutlines: Seq[ProofOutline.Step],
    context: ProvingContext,
    nextReference: Int
  ): (AssumptionStep, ProvingContext) = {
    val referencedAssumption = ReferencedAssertion(ProvenStatement.withNoConditions(assumption), DirectReference(nextReference, assumption.html))
    val contextWithAssumption = context.copy(
      provenAssertions = context.provenAssertions :+ referencedAssumption,
      assumptions = context.assumptions :+ assumption)
    val (substeps, _) = proveSteps(
      substepOutlines,
      Nil,
      contextWithAssumption,
      nextReference + 1)
    val assumptionStep = AssumptionStep(assumption, substeps)
    val newProvenDeductions = assumptionStep.steps.zipWithIndex.collect {
      case (StepWithProvenStatement(deduction), index) =>
        ReferencedDeduction(assumption, deduction, DeducedReference(nextReference, nextReference + index + 1))
    }
    (assumptionStep, context.copy(provenDeductions = context.provenDeductions ++ newProvenDeductions))
  }

  private def proveAssertionStep(
    stepOutline: ProofOutline.AssertionStep,
    context: ProvingContext,
    nextReference: Int
  ):  Option[(StepWithProvenStatement, ProvingContext)] = {
    for {
      assertionStep <- Prover(
        stepOutline.assertion,
        stepOutline.nonArbitraryVariables,
        stepOutline.nonDistinctVariables,
        context,
        stepOutline.debug
      ).proveAssertion()
    } yield {
      val newProvenAssertion = ReferencedAssertion(
        assertionStep.provenStatement,
        DirectReference(nextReference, assertionStep.provenStatement.statement.html))
      (assertionStep, context.copy(provenAssertions = context.provenAssertions :+ newProvenAssertion))
    }
  }
}
