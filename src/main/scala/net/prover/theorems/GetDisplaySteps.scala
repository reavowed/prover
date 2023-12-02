package net.prover.theorems

import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.Step

object GetDisplaySteps {
  def apply(proof: Proof): Seq[DisplayStep] = {
    apply(proof.steps, Nil)
  }

  def apply(steps: Seq[Step], stepPath: Seq[Int], offset: Int = 0): Seq[DisplayStep] = {
    steps.mapWithIndex((step, index) => apply(step, stepPath :+ (index + offset)))
  }

  def apply(step: Step, stepPath: Seq[Int]): DisplayStep = {
    step match {
      case step: Step.Target =>
        DisplayStep.Target(step.statement, stepPath)
      case step: Step.Assertion =>
        DisplayStep.Assertion(step.statement, step.inference, stepPath, step.premises)
      case step: Step.Deduction =>
        DisplayStep.Deduction(step.assumption, step.statement, stepPath, apply(step.substeps, stepPath))
      case step: Step.Generalization =>
        DisplayStep.Generalization(step.variableName, step.statement, apply(step.substeps, stepPath), stepPath)
      case step: Step.Naming =>
        DisplayStep.Naming(
          step.variableName,
          step.assumption,
          step.statement,
          step.inference,
          stepPath,
          step.premises,
          apply(step.substeps, stepPath))
      case step: Step.Subproof =>
        DisplayStep.Subproof(step.name, step.statement, stepPath, apply(step.substeps, stepPath))
      case step: Step.Elided =>
        step.highlightedInference match {
          case Some(i) =>
            DisplayStep.ElidedInference(
              step.statement,
              i,
              stepPath,
              apply(step.substeps, stepPath))
          case None =>
            DisplayStep.ElidedWithDescription(
              step.statement,
              step.description,
              stepPath,
              apply(step.substeps, stepPath))
        }
      case step: Step.ExistingStatementExtraction =>
        DisplayStep.ElidedWithDescription(
          step.statement,
          Some("Extraction from previous step"),
          stepPath,
          apply(step.substeps, stepPath))
      case step: Step.InferenceExtraction =>
        DisplayStep.ElidedInference(
          step.statement,
          step.inference,
          stepPath,
          apply(step.substeps, stepPath))
      case step: Step.WrappedInferenceApplication =>
        DisplayStep.ElidedInference(
          step.statement,
          step.inference,
          stepPath,
          apply(step.substeps, stepPath))
      case step: Step.WrappedPremiseDerivation =>
        DisplayStep.ElidedWithDescription(
          step.statement,
          Some("Premise derivation"), // TODO: Could potentially point at an inference
          stepPath,
          apply(step.substeps, stepPath))
      case step: Step.InferenceWithPremiseDerivations =>
        DisplayStep.ElidedInference(
          step.statement,
          step.inference,
          stepPath,
          apply(step.substeps, stepPath))
    }
  }
}
