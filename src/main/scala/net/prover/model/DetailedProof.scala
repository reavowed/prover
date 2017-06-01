package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

case class DetailedProof(steps: Seq[DetailedProof.Step])

object DetailedProof {
  sealed trait Step
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
    extends Step
  case class NamingStep(
      variable: TermVariable,
      assumptionStep: AssumptionStep,
      assertionStep: AssertionStep)
    extends StepWithProvenStatement {
    override def provenStatement: ProvenStatement = assertionStep.provenStatement
  }
  case class AssertionStep(
      provenStatement: ProvenStatement,
      inferenceName: String,
      references: Seq[Reference])
    extends StepWithProvenStatement

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
            val newProvenDeductions = substeps.zipWithIndex.collect {
              case (StepWithProvenStatement(deduction), index) =>
                ReferencedDeduction(assumption, deduction, DeducedReference(nextReference, nextReference + index + 1))
            }
            (provenAssertions, provenDeductions ++ newProvenDeductions)
          case NamingStep(_, _, assertion) =>
            val newProvenAssertion = ReferencedAssertion(assertion.provenStatement, DirectReference(nextReference))
            (provenAssertions :+ newProvenAssertion, provenDeductions)
          case AssertionStep(provenStatement, _, _) =>
            val newProvenAssertion = ReferencedAssertion(provenStatement, DirectReference(nextReference))
            (provenAssertions :+ newProvenAssertion, provenDeductions)
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
        proveAssumptionStep(assumption, substepOutlines, provenAssertions, provenDeductions, premises, assumptions, nextReference)
      case ProofOutline.NamingStep(variable, namingStatement, substepOutlines) =>
        val assumptionStep = proveAssumptionStep(namingStatement, substepOutlines, provenAssertions, provenDeductions, premises, assumptions, nextReference)
        val finalAssertionStatement = assumptionStep.steps match {
          case _ :+ StepWithProvenStatement(statement) =>
            statement
          case _ =>
            throw new Exception("Let step must end with an assertion")
        }
        val assertionStep = proveAssertionStep(
          finalAssertionStatement.statement,
          Set.empty,
          Set.empty,
          false,
          provenAssertions,
          provenDeductions :+ ReferencedDeduction(namingStatement, finalAssertionStatement, DeducedReference(nextReference, nextReference + assumptionStep.steps.length)),
          premises,
          assumptions,
          nextReference)
        NamingStep(variable, assumptionStep, assertionStep)
      case ProofOutline.AssertionStep(assertion, nonArbitraryVariables, nonDistinctVariables, debug) =>
        proveAssertionStep(assertion, nonArbitraryVariables, nonDistinctVariables, debug, provenAssertions, provenDeductions, premises, assumptions, nextReference)
    }
  }

  private def proveAssumptionStep(
    assumption: Statement,
    substepOutlines: Seq[ProofOutline.Step],
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction],
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    nextReference: Int)(
    implicit context: Context
  ): AssumptionStep = {
    val substeps = proveSteps(
      substepOutlines,
      Nil,
      provenAssertions :+ ReferencedAssertion(ProvenStatement.withNoConditions(assumption), DirectReference(nextReference)),
      provenDeductions,
      premises,
      assumptions :+ assumption,
      nextReference + 1)
    AssumptionStep(assumption, substeps)
  }

  private def proveAssertionStep(
    assertion: Statement,
    nonArbitraryVariables: Set[TermVariable],
    nonDistinctVariables: Set[(Variable, Variable)],
    debug: Boolean,
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction],
    premises: Seq[Premise],
    assumptions: Seq[Statement],
    nextReference: Int)(
    implicit context: Context
  ): AssertionStep = {
    Prover(assertion, nonArbitraryVariables, nonDistinctVariables, provenAssertions, provenDeductions, premises, assumptions, debug).proveAssertion()
  }
}
