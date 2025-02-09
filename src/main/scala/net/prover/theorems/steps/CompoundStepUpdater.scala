package net.prover.theorems.steps

import net.prover.entries.{ProofWithContext, StepWithContext, StepsWithContext, TypedStepWithContext}
import net.prover.model.definitions.KnownStatement
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction}
import net.prover.proving.rewrite.RewritePremise
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import scalaz.Monad
import scalaz.Scalaz._

abstract class CompoundStepUpdater[F[_] : Monad] {
  def apply(stepsWithContext: StepsWithContext): F[List[Step]] = {
    stepsWithContext.stepsWithContexts.toList.map(apply).sequence
  }

  def apply(stepWithContext: StepWithContext): F[Step] = {
    stepWithContext.step match {
      case step: Step.TargetStep => updateTarget(step, stepWithContext)
      case step: Step.AssertionStep => updateAssertion(step, stepWithContext)
      case step: Step.DeductionStep => updateDeduction(step, stepWithContext)
      case step: Step.GeneralizationStep => updateGeneralization(step, stepWithContext)
      case step: Step.NamingStep => updateNaming(step, stepWithContext)
      case step: Step.SubproofStep => updateSubProof(step, stepWithContext)
      case step: Step.ElidedStep => updateElided(step, stepWithContext)
      case step: Step.Autogenerated => updateAutogenerated(step, stepWithContext)
      case step: Step.InferenceExtractionStep => updateInferenceExtraction(step, stepWithContext)
      case step: Step.InferenceWithPremiseDerivationsStep => updateInferenceWithPremiseDerivations(step, stepWithContext)
      case step: Step.ExistingStatementExtractionStep => updateExistingStatementExtraction(step, stepWithContext)
      case step: Step.RewriteStep => updateRewriteStep(step, stepWithContext)
    }
  }

  def updateTarget(step: Step.TargetStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, stepWithContext)
    } yield Step.TargetStep(newStatement)
  }
  def updateAssertion(step: Step.AssertionStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, stepWithContext)
      newInference <- updateInference(step.inference, stepWithContext)
      newPremises <- step.premises.toList.map(updatePremise(_, stepWithContext)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, stepWithContext)
    } yield Step.AssertionStep(newStatement, newInference, newPremises, newSubstitutions)
  }
  private def updateAssertion(step: Step.AssertionStep, stepContext: StepContext, proofWithContext: ProofWithContext): F[Step.AssertionStep] = {
    updateAssertion(step, TypedStepWithContext(step, proofWithContext)(implicitly, stepContext))
      .map(_.asInstanceOf[Step.AssertionStep])
  }
  private def updateAssertions(steps: Seq[Step.AssertionStep], stepContext: StepContext, proofWithContext: ProofWithContext): F[Seq[Step.AssertionStep]] = {
    updateAll(
      steps,
      stepContext,
      updateAssertion(_, _, proofWithContext),
      _.addStep(_))
  }
  def updateDeduction(step: Step.DeductionStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, stepWithContext)
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      deductionDefinition <- updateDeductionDefinition(step.deductionDefinition)
    } yield Step.DeductionStep(newAssumption, newSubsteps, deductionDefinition)
  }
  def updateGeneralization(step: Step.GeneralizationStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      generalizationDefinition <- updateGeneralizationDefinition(step.generalizationDefinition)
    } yield Step.GeneralizationStep(step.variableName, newSubsteps, generalizationDefinition)
  }
  def updateNaming(step: Step.NamingStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, stepWithContext.stepContext.addBoundVariable(step.variableName))
      newStatement <- updateStatement(step.statement, stepWithContext)
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      newInference <- updateInference(step.inference, stepWithContext)
      newPremises <- step.premises.toList.map(updatePremise(_, stepWithContext)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, stepWithContext)
      deductionDefinition <- updateDeductionDefinition(step.deductionDefinition)
      generalizationDefinition <- updateGeneralizationDefinition(step.generalizationDefinition)
    } yield Step.NamingStep(
      step.variableName,
      newAssumption,
      newStatement,
      newSubsteps,
      newInference,
      newPremises,
      newSubstitutions,
      generalizationDefinition,
      deductionDefinition)
  }
  def updateSubProof(step: Step.SubproofStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
    } yield Step.SubproofStep(step.name, newSubsteps)
  }
  def updateElided(step: Step.ElidedStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      newHighlightedInference <- step.highlightedInference.map(updateInference(_, stepWithContext)).sequence
    } yield Step.ElidedStep(newSubsteps, newHighlightedInference, step.description)
  }
  def updateAutogenerated(step: Step.Autogenerated, stepWithContext: StepWithContext) = {
    apply(stepWithContext.forSubsteps(step)).map(step.replaceSubsteps(_)(stepWithContext.stepProvingContext))
  }
  def updateInferenceExtraction(step: Step.InferenceExtractionStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newInferenceExtraction <- updateAppliedInferenceExtraction(step.inferenceExtraction, stepWithContext.stepContext, stepWithContext.proofWithContext)
    } yield Step.InferenceExtractionStep(newInferenceExtraction)
  }
  def updateInferenceWithPremiseDerivations(step: Step.InferenceWithPremiseDerivationsStep, stepWithContext: StepWithContext): F[Step] = {
    Step.InferenceWithPremiseDerivationsStep.builder(
      sc => updateKnownStatements(step.premises, sc, stepWithContext.proofWithContext),
      sc => apply(TypedStepWithContext(step.assertionStep, stepWithContext.proofWithContext)(implicitly, sc))
        .map(_.asInstanceOf[Step.InferenceApplicationWithoutPremises]))(
      implicitly,
      stepWithContext.stepContext).map(_.asInstanceOf[Step])
  }
  def updateExistingStatementExtraction(step: Step.ExistingStatementExtractionStep, stepWithContext: StepWithContext): F[Step] = {
    Step.ExistingStatementExtractionStep.builder(
      sc => updateKnownStatements(step.premises, sc, stepWithContext.proofWithContext),
      sc => updateAppliedExtraction(step.extraction, sc, stepWithContext.proofWithContext))(
      implicitly,
      stepWithContext.stepContext).map(_.asInstanceOf[Step])
  }
  private def updateRewriteStep(step: Step.RewriteStep, stepWithContext: StepWithContext): F[Step] = {
    for {
      newPremise <- updateRewritePremise(step.premise, stepWithContext.stepContext.forChild(), stepWithContext.proofWithContext)
      newSubstitutionStep <- updateAssertion(
        step.substitutionStep,
        stepWithContext.stepContext.forChild().addSteps(newPremise.toProofSteps),
        stepWithContext.proofWithContext)
    } yield Step.RewriteStep(newPremise, newSubstitutionStep)
  }

  def updateAppliedInferenceExtraction(appliedInferenceExtraction: AppliedInferenceExtraction, stepContext: StepContext, proofWithContext: ProofWithContext): F[AppliedInferenceExtraction] = {
    for {
      newAssertionStep <- updateAssertion(
        appliedInferenceExtraction.assertionStep,
        stepContext.forChild(),
        proofWithContext)
      newExtraction <- updateAppliedExtraction(
        appliedInferenceExtraction.extraction,
        stepContext.forChild().addStep(newAssertionStep),
        proofWithContext)
    } yield AppliedInferenceExtraction(newAssertionStep, newExtraction)
  }
  def updateAppliedExtractionStep(appliedExtractionStep: AppliedExtractionStep, stepContext: StepContext, proofWithContext: ProofWithContext): F[AppliedExtractionStep] = {
    appliedExtractionStep match {
      case AppliedExtractionStep.Assertion(step) =>
        updateAssertion(step, stepContext, proofWithContext).map(AppliedExtractionStep.Assertion(_))
      case AppliedExtractionStep.DefinitionDeconstruction(deconstructionStep, additionalSteps) =>
        val innerContext = stepContext.forChild()
        for {
          newDeconstructionStep <- updateAssertion(deconstructionStep, innerContext, proofWithContext)
          newAdditionalSteps <- updateAssertions(additionalSteps, innerContext.addStep(deconstructionStep), proofWithContext)
        } yield AppliedExtractionStep.DefinitionDeconstruction(newDeconstructionStep, newAdditionalSteps)
    }
  }
  def updateAppliedExtraction(appliedExtraction: AppliedExtraction, stepContext: StepContext, proofWithContext: ProofWithContext): F[AppliedExtraction] = {
    for {
      newExtractionSteps <- updateAll[AppliedExtractionStep](
        appliedExtraction.extractionSteps,
        stepContext,
        updateAppliedExtractionStep(_, _, proofWithContext),
        (sc, s) => sc.addStep(s.toProofStep))
      newRewriteSteps <- updateAssertions(
        appliedExtraction.chainedRewriteSteps,
        stepContext.addSteps(newExtractionSteps.map(_.toProofStep)),
        proofWithContext)
    } yield AppliedExtraction(newExtractionSteps, newRewriteSteps)
  }
  def updateRewritePremise(rewritePremise: RewritePremise, stepContext: StepContext, proofWithContext: ProofWithContext): F[RewritePremise] = {
    rewritePremise match {
      case RewritePremise.Known(knownStatement) =>
        updateKnownStatement(knownStatement, stepContext, proofWithContext).map(RewritePremise.Known(_))
      case RewritePremise.ByInference(premises, extraction) =>
        for {
          newPremises <- updateKnownStatements(premises, stepContext.forChild(), proofWithContext)
          newExtraction <- updateAppliedInferenceExtraction(extraction, stepContext.forChild().addSteps(newPremises.flatMap(_.toProofSteps)), proofWithContext)
        } yield RewritePremise.ByInference(newPremises, newExtraction)
    }
  }
  def updateKnownStatements(knownStatements: Seq[KnownStatement], stepContext: StepContext, proofWithContext: ProofWithContext): F[Seq[KnownStatement]] = {
    updateAll[KnownStatement](
      knownStatements,
      stepContext,
      updateKnownStatement(_, _, proofWithContext),
      (sc, ks) => sc.addSteps(ks.toProofSteps))
  }
  def updateKnownStatement(knownStatement: KnownStatement, stepContext: StepContext, proofWithContext: ProofWithContext): F[KnownStatement] = {
    for {
      newStatement <- updateStatement(knownStatement.statement, stepContext)
      newDerivation <- updateSimpleDerivation(knownStatement.derivation, stepContext, proofWithContext)
    } yield KnownStatement(newStatement, newDerivation)
  }
  def updateSimpleDerivation(simpleDerivation: SimpleDerivation, stepContext: StepContext, proofWithContext: ProofWithContext): F[SimpleDerivation] = {
    updateAll[SimpleDerivationStep](
      simpleDerivation.steps,
      stepContext,
      updateSimpleDerivationStep(_, _, proofWithContext),
      (sc, s) => sc.addStep(s.toProofStep)
    ).map(SimpleDerivation(_))
  }
  def updateSimpleDerivationStep(simpleDerivationStep: SimpleDerivationStep, stepContext: StepContext, proofWithContext: ProofWithContext): F[SimpleDerivationStep] = {
    simpleDerivationStep match {
      case SimpleDerivationStep.Assertion(step) =>
        updateAssertion(step, stepContext, proofWithContext)
          .map(SimpleDerivationStep.Assertion(_))
      case SimpleDerivationStep.DefinitionDeconstruction(deconstructionStep, additionalSteps) =>
        for {
          newDeconstructionStep <- updateAssertion(deconstructionStep, stepContext, proofWithContext)
          newAdditionalSteps <- updateAssertions(additionalSteps, stepContext.addStep(newDeconstructionStep), proofWithContext)
        } yield SimpleDerivationStep.DefinitionDeconstruction(newDeconstructionStep, newAdditionalSteps)
      case SimpleDerivationStep.InferenceExtraction(extraction) =>
        updateAppliedInferenceExtraction(extraction, stepContext, proofWithContext)
          .map(SimpleDerivationStep.InferenceExtraction(_))
    }
  }
  def updateStatement(statement: Statement, substitutionContext: SubstitutionContext): F[Statement] = Monad[F].point(statement)
  def updateInference(inference: Inference.Summary, stepWithContext: StepWithContext): F[Inference.Summary] = Monad[F].point(inference)
  def updatePremise(premise: Premise, stepWithContext: StepWithContext): F[Premise] = Monad[F].point(premise)
  def updateSubstitutions(substitutions: Substitutions, stepWithContext: StepWithContext): F[Substitutions] = Monad[F].point(substitutions)
  def updateDeductionDefinition(deductionDefinition: DeductionDefinition): F[DeductionDefinition] = Monad[F].point(deductionDefinition)
  def updateGeneralizationDefinition(generalizationDefinition: GeneralizationDefinition): F[GeneralizationDefinition] = Monad[F].point(generalizationDefinition)

  private def updateAll[T](
    items: Seq[T],
    stepContext: StepContext,
    updateOne: (T, StepContext) => F[T],
    updateStepContext: (StepContext, T) => StepContext
  ): F[Seq[T]] = {
    items.toList.foldLeftM((List.empty[T], stepContext)) {
      case ((updatedItems, stepContext), item) =>
        updateOne(item, stepContext).map(updatedItem => (updatedItems :+ updatedItem, updateStepContext(stepContext, updatedItem)))
    }.map(_._1)
  }
}
