package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model._
import net.prover.model.definitions.KnownStatement
import net.prover.model.proof.Step
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction, ExtractionApplier, ExtractionCalculator}
import net.prover.proving.premiseFinding.DerivationFinder
import net.prover.proving.rewrite.RewritePremise
import net.prover.theorems.CompoundTheoremUpdater
import scalaz.Scalaz._

object ReplaceElidedSteps extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply)
  }

  override def updateElided(step: Step.ElidedStep, stepWithContext: StepWithContext): Step = {
    replaceWithInferenceExtraction(step, stepWithContext) orElse
      replaceWithExistingStatementExtraction(step, stepWithContext) orElse
      replaceWithRewrite(step, stepWithContext) getOrElse
      super.updateElided(step, stepWithContext)
  }

  private def replaceWithInferenceExtraction(baseStep: Step.ElidedStep, stepWithContext: StepWithContext): Option[Step] = {
    for {
      inference <- baseStep.highlightedInference
      reprovedStep <- Reprove(stepWithContext.withStep(baseStep), inference)
    } yield reprovedStep
  }

  private def replaceWithExistingStatementExtraction(step: Step.ElidedStep, stepWithContext: StepWithContext): Option[Step.ExistingStatementExtractionStep] = {
    for {
      assertionSteps <- step.substeps.map(_.asOptionalInstanceOf[Step.AssertionStep]).toList.sequence
      firstAssertion <- assertionSteps.headOption
      mainPremise <- firstAssertion.premises.headOption.map(_.statement)
      _ <- ExtractionCalculator.getPremiseExtractions(mainPremise)(stepWithContext.stepContext, stepWithContext.provingContext)
        .find(_.extractionDetails.allSteps.map(_.inference) == assertionSteps.map(_.inference))
      appliedExtractionSteps = ExtractionApplier.groupStepsByDefinition(assertionSteps)(stepWithContext.provingContext)
    } yield Step.ExistingStatementExtractionStep(AppliedExtraction(appliedExtractionSteps, Nil))
  }

  private def replaceWithRewrite(step: Step.ElidedStep, stepWithContext: StepWithContext): Option[Step.RewriteStep] = {
    import stepWithContext.stepProvingContext
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
