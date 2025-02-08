package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model._
import net.prover.model.definitions.KnownStatement
import net.prover.model.proof.Step
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction, ExtractionApplier, ExtractionCalculator}
import net.prover.proving.premiseFinding.DerivationFinder
import net.prover.proving.rewrite.RewritePremise
import net.prover.theorems.{CompoundTheoremUpdater, GetReferencedPremises, RecalculateReferences}
import scalaz.Scalaz._

object ReplaceElidedSteps extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply)
  }

  override def updateElided(step: Step.ElidedStep, stepWithContext: StepWithContext): Step = {
    replaceWithInferenceExtraction(step, stepWithContext) orElse
      replaceWithExistingStatementExtraction(step, stepWithContext) orElse
      replaceWithInnerExistingStatementExtraction(step, stepWithContext) orElse
      replaceWithRewrite(step, stepWithContext) getOrElse
      super.updateElided(step, stepWithContext)
  }

  private def replaceWithInferenceExtraction(baseStep: Step.ElidedStep, stepWithContext: StepWithContext): Option[Step] = {
    for {
      inference <- baseStep.highlightedInference
      reprovedStep <- Reprove(stepWithContext.withStep(baseStep), inference)
    } yield reprovedStep
  }

  private def replaceWithExistingStatementExtraction(step: Step.ElidedStep, stepWithContext: StepWithContext): Option[Step] = {
    val premises = GetReferencedPremises(stepWithContext)
    for {
      assertionSteps <- step.substeps.map(_.asOptionalInstanceOf[Step.AssertionStep]).toList.sequence
      result <- Reprove.reproveExistingStatementExtraction(
        premises,
        step.statement,
        assertionSteps.map(_.inference))(stepWithContext.stepProvingContext)
      (recalculatedResult, _) <- RecalculateReferences(stepWithContext.withStep(result)).toOption
    } yield recalculatedResult
  }
  private def replaceWithInnerExistingStatementExtraction(step: Step.ElidedStep, stepWithContext: StepWithContext): Option[Step] = {
    for {
      lastStepWithContext <- stepWithContext.forSubsteps(step).stepsWithContexts.lastOption
      lastStep <- lastStepWithContext.step.asOptionalInstanceOf[Step.ElidedStep]
      premises = GetReferencedPremises(lastStepWithContext)
      assertionSteps <- lastStep.substeps.map(_.asOptionalInstanceOf[Step.AssertionStep]).toList.sequence
      result <- Reprove.reproveExistingStatementExtraction(
        premises,
        step.statement,
        assertionSteps.map(_.inference))(stepWithContext.stepProvingContext)
      (recalculatedResult, _) <- RecalculateReferences(stepWithContext.withStep(result)).toOption
    } yield recalculatedResult
  }
    private def replaceWithRewrite(step: Step.ElidedStep, stepWithContext: StepWithContext): Option[Step.RewriteStep] = {
    for {
      (previousSteps, lastStep) <- step.substeps.initAndLastOption
      lastAssertion <- lastStep.asOptionalInstanceOf[Step.AssertionStep]
      _ <- stepWithContext.provingContext.substitutions.find(_.inference == lastAssertion.inference) orElse
        stepWithContext.provingContext.expansions.find(_.inference == lastAssertion.inference)
      firstStep <- previousSteps.single
      firstAssertion <- firstStep.asOptionalInstanceOf[Step.AssertionStep]
      if step.highlightedInference.contains(firstAssertion.inference)
    } yield Step.RewriteStep(
      RewritePremise.ByInference(
        Nil,
        AppliedInferenceExtraction(firstAssertion, AppliedExtraction(Nil, Nil))),
      lastAssertion)
  }
}
