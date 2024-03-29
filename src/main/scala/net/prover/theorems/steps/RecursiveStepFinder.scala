package net.prover.theorems.steps

import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import scalaz.Monoid
import scalaz.Scalaz._

abstract class RecursiveStepFinder[T : Monoid] {
  def apply(theorem: Theorem): T = theorem.proofs.toList.foldMap(apply)
  def apply(proof: Proof): T = apply(proof.steps)
  def apply(steps: Seq[Step]): T = steps.toList.foldMap(apply)

  def apply(step: Step): T = {
    step match {
      case step: Step.Target => apply(step)
      case step: Step.Assertion => apply(step)
      case step: Step.Deduction => apply(step)
      case step: Step.Generalization => apply(step)
      case step: Step.Naming => apply(step)
      case step: Step.SubProof => apply(step)
      case step: Step.Elided => apply(step)
      case step: Step.Autogenerated => apply(step.substeps)
    }
  }

  def apply(step: Step.Target): T = {
    apply(step.statement)
  }
  def apply(step: Step.Assertion): T = {
    apply(step.statement) |+|
      apply(step.inference) |+|
      step.premises.toList.foldMap(apply) |+|
      apply(step.substitutions)
  }
  def apply(step: Step.Deduction): T = {
    apply(step.assumption) |+|
      apply(step.substeps) |+|
      apply(step.deductionDefinition)
  }
  def apply(step: Step.Generalization): T = {
    apply(step.substeps) |+|
      apply(step.generalizationDefinition)
  }
  def apply(step: Step.Naming): T = {
    apply(step.assumption) |+|
      apply(step.substeps) |+|
      apply(step.statement) |+|
      apply(step.inference) |+|
      step.premises.toList.foldMap(apply) |+|
      apply(step.substitutions) |+|
      apply(step.deductionDefinition) |+|
      apply(step.generalizationDefinition)
  }
  def apply(step: Step.SubProof): T = {
    apply(step.substeps)
  }
  def apply(step: Step.Elided): T = {
    apply(step.substeps) |+| step.highlightedInference.map(apply).getOrElse(mzero[T])
  }

  def apply(statement: Statement): T
  def apply(inference: Inference.Summary): T
  def apply(premise: Premise): T
  def apply(substitutions: Substitutions): T
  def apply(deductionDefinition: DeductionDefinition): T
  def apply(generalizationDefinition: GeneralizationDefinition): T
}
