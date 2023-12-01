package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model._
import net.prover.model.proof.Step
import net.prover.proving.extraction.ExtractionCalculator
import net.prover.theorems.CompoundTheoremUpdater
import scalaz.Scalaz._

object ReplaceElidedSteps extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply)
  }

  override def updateElided(step: Step.Elided, stepWithContext: StepWithContext): Step = {
    replaceWithInferenceExtraction(step, stepWithContext) orElse
    replaceWithExistingStatementExtraction(step, stepWithContext) getOrElse
      super.updateElided(step, stepWithContext)
  }

  private def replaceWithInferenceExtraction(baseStep: Step.Elided, stepWithContext: StepWithContext): Option[Step] = {
    for {
      inference <- baseStep.highlightedInference
      reprovedStep <- Reprove(stepWithContext.withStep(baseStep), inference)
    } yield reprovedStep
  }

  private def replaceWithExistingStatementExtraction(step: Step.Elided, stepWithContext: StepWithContext): Option[Step.ExistingStatementExtraction] = {
    for {
      assertionSteps <- step.substeps.map(_.asOptionalInstanceOf[Step.Assertion]).toList.sequence
      firstAssertion <- assertionSteps.headOption
      mainPremise <- firstAssertion.premises.headOption.map(_.statement)
      _ <- ExtractionCalculator.getPremiseExtractions(mainPremise)(stepWithContext.stepContext, stepWithContext.provingContext)
        .find(_.innerExtraction.derivation.map(_.inference) == assertionSteps.map(_.inference))
    } yield Step.ExistingStatementExtraction(assertionSteps)
  }
}
