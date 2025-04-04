package net.prover.theorems.steps

import net.prover.model.definitions.KnownStatement
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction}
import net.prover.proving.rewrite.RewritePremise
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import scalaz.Monoid
import scalaz.Scalaz._

abstract class RecursiveStepFinder[T : Monoid] {
  def apply(theorem: Theorem): T = theorem.proofs.toList.foldMap(apply)
  def apply(proof: Proof): T = apply(proof.steps)
  def apply(steps: Seq[Step]): T = steps.toList.foldMap(apply)

  def apply(step: Step): T = {
    step match {
      case step: Step.TargetStep => apply(step)
      case step: Step.AssertionStep => apply(step)
      case step: Step.DeductionStep => apply(step)
      case step: Step.GeneralizationStep => apply(step)
      case step: Step.NamingStep => apply(step)
      case step: Step.SubproofStep => apply(step)
      case step: Step.ElidedStep => apply(step)
      case step: Step.InferenceExtractionStep => apply(step)
      case step: Step.InferenceWithPremiseDerivationsStep => apply(step)
      case step: Step.ExistingStatementExtractionStep => apply(step)
      case step: Step.RewriteStep => apply(step)
      case step: Step.Autogenerated => apply(step.substeps)
    }
  }

  def apply(step: Step.TargetStep): T = {
    apply(step.statement)
  }
  def apply(step: Step.AssertionStep): T = {
    apply(step.statement) |+|
      apply(step.inference) |+|
      step.premises.toList.foldMap(apply) |+|
      apply(step.substitutions)
  }
  def apply(step: Step.DeductionStep): T = {
    apply(step.assumption) |+|
      apply(step.substeps) |+|
      apply(step.deductionDefinition)
  }
  def apply(step: Step.GeneralizationStep): T = {
    apply(step.substeps) |+|
      apply(step.generalizationDefinition)
  }
  def apply(step: Step.NamingStep): T = {
    apply(step.assumption) |+|
      apply(step.substeps) |+|
      apply(step.statement) |+|
      apply(step.inference) |+|
      step.premises.toList.foldMap(apply) |+|
      apply(step.substitutions) |+|
      apply(step.deductionDefinition) |+|
      apply(step.generalizationDefinition)
  }
  def apply(step: Step.SubproofStep): T = {
    apply(step.substeps)
  }
  def apply(step: Step.ElidedStep): T = {
    apply(step.substeps) |+| step.highlightedInference.map(apply).getOrElse(mzero[T])
  }
  def apply(step: Step.InferenceExtractionStep): T = {
    apply(step.inferenceExtraction.assertionStep) |+| apply(step.inferenceExtraction.extraction)
  }
  def apply(step: Step.InferenceWithPremiseDerivationsStep): T = {
    step.premises.toList.foldMap(apply) |+| apply(step.assertionStep)
  }
  def apply(step: Step.ExistingStatementExtractionStep): T = {
    apply(step.extraction)
  }
  def apply(step: Step.RewriteStep): T = {
    apply(step.premise) |+| apply(step.substitutionStep)
  }

  def apply(extraction: AppliedExtraction): T = {
    extraction.extractionSteps.toList.foldMap {
      case AppliedExtractionStep.Assertion(step) =>
        apply(step)
      case AppliedExtractionStep.DefinitionDeconstruction(deconstructionStep, additionalSteps) =>
        apply(deconstructionStep) |+| additionalSteps.toList.foldMap(apply)
    } |+| apply(extraction.chainedRewriteSteps)
  }
  def apply(premise: RewritePremise): T = {
    premise match {
      case RewritePremise.Known(knownStatement) =>
        apply(knownStatement)
      case RewritePremise.ByInference(premises, extraction) =>
        premises.toList.foldMap(apply) |+| apply(extraction)
    }
  }
  def apply(knownStatement: KnownStatement): T = {
    apply(knownStatement.statement) |+| apply(knownStatement.derivation)
  }
  def apply(appliedInferenceExtraction: AppliedInferenceExtraction): T = {
    apply(appliedInferenceExtraction.assertionStep) |+| apply(appliedInferenceExtraction.extraction)
  }
  def apply(simpleDerivation: SimpleDerivation): T = {
    simpleDerivation.steps.toList.foldMap {
      case SimpleDerivationStep.Assertion(assertion) =>
        apply(assertion)
      case SimpleDerivationStep.DefinitionDeconstruction(deconstructionStep, additionalSteps) =>
        apply(deconstructionStep) |+| additionalSteps.toList.foldMap(apply)
      case SimpleDerivationStep.InferenceExtraction(extraction) =>
        apply(extraction)
    }
  }

  def apply(statement: Statement): T
  def apply(inference: Inference.Summary): T
  def apply(premise: Premise): T
  def apply(substitutions: Substitutions): T
  def apply(deductionDefinition: DeductionDefinition): T
  def apply(generalizationDefinition: GeneralizationDefinition): T
}
