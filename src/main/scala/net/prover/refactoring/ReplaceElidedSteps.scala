package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model._
import net.prover.model.proof.Step
import net.prover.proving.extraction.{AppliedExtraction, ExtractionApplier, ExtractionCalculator}
import net.prover.theorems.CompoundTheoremUpdater
import scalaz.Scalaz._

object ReplaceElidedSteps extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply)
  }

  override def updateElided(step: Step.ElidedStep, stepWithContext: StepWithContext): Step = {
    replaceWithInferenceExtraction(step, stepWithContext) orElse
    replaceWithExistingStatementExtraction(step, stepWithContext) getOrElse
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
}
