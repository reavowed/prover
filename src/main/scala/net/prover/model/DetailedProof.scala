package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

case class DetailedProof(steps: Seq[DetailedProof.Step])

object DetailedProof {
  sealed trait Step
  case class AssumptionStep(
      assumption: Statement,
      steps: Seq[Step])
    extends Step
  case class AssertionStep(
      provenStatement: ProvenStatement,
      inference: Inference,
      references: Seq[Reference],
      substitutions: Substitutions)
    extends Step

  sealed trait Reference
  case class DirectReference(index: Int) extends Reference
  case class DeducedReference(antecedentIndex: Int, consequentIndex: Int) extends Reference
  case class InferenceReference(inference: Inference, premiseReferences: Seq[Reference], substitutions: Substitutions) extends Reference

  case class ReferencedAssertion(provenStatement: ProvenStatement, reference: DirectReference)
  case class ReferencedDeduction(assumption: Statement, deduction: ProvenStatement, reference: Reference)

  def fillInOutline(
    premises: Seq[Premise],
    proofOutline: ProofOutline)(
    implicit context: Context
  ): DetailedProof = {
    val premiseAssertions = premises.zipWithIndex.collect {
      case (DirectPremise(premise), index) =>
        ReferencedAssertion(ProvenStatement.withNoConditions(premise), DirectReference(index))
    }
    val premiseDeductions = premises.zipWithIndex.collect {
      case (DeducedPremise(assumption, conclusion), index) =>
        ReferencedDeduction(assumption, ProvenStatement.withNoConditions(conclusion), DirectReference(index))
    }
    val detailedSteps = proveSteps(
      proofOutline.steps,
      Nil,
      premiseAssertions,
      premiseDeductions,
      premises,
      Nil,
      premises.length)
    DetailedProof(detailedSteps)
  }

  private def proveSteps(
    stepOutlines: Seq[ProofOutline.Step],
    accumulatedSteps: Seq[Step],
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction],
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    nextReference: Int)(
    implicit context: Context
  ): Seq[Step] = {
    stepOutlines match {
      case Nil =>
        accumulatedSteps
      case stepOutline +: otherStepOutlines =>
        val step = proveStep(stepOutline, provenAssertions, provenDeductions, premises, assumptions, nextReference)
        val (updatedAssertions, updatedDeductions) = step match {
          case AssumptionStep(assumption, substeps) =>
            val newDeductions = substeps.zipWithIndex.collect {
              case (AssertionStep(deduction, _, _, _), index) =>
                ReferencedDeduction(assumption, deduction, DeducedReference(nextReference, index))
            }
            (provenAssertions, provenDeductions ++ newDeductions)
          case AssertionStep(provenStatement, _, _, _) =>
            val newAssertion = ReferencedAssertion(provenStatement, DirectReference(nextReference))
            (provenAssertions :+ newAssertion, provenDeductions)
        }
        proveSteps(
          otherStepOutlines,
          accumulatedSteps :+ step,
          updatedAssertions,
          updatedDeductions,
          premises,
          assumptions,
          nextReference + 1)
    }
  }

  private def proveStep(
    stepOutline: ProofOutline.Step,
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction],
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    nextReference: Int)(
    implicit context: Context
  ): Step = {
    stepOutline match {
      case ProofOutline.AssumptionStep(assumption, substepOutlines) =>
        val substeps = proveSteps(
          substepOutlines,
          Nil,
          provenAssertions :+ ReferencedAssertion(ProvenStatement.withNoConditions(assumption), DirectReference(nextReference)),
          provenDeductions,
          premises,
          assumptions :+ assumption,
          nextReference + 1)
        AssumptionStep(assumption, substeps)
      case ProofOutline.AssertionStep(assertion, conditions) =>
        Prover(assertion, conditions, provenAssertions, provenDeductions, premises, assumptions).proveAssertion()
    }
  }
}
