package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model.proof.{ProofHelper, Step}
import net.prover.proving.extraction.{ExtractionHelper, SubstatementExtractor}
import net.prover.proving.premiseFinding.DerivationOrTargetFinder
import net.prover.theorems.CompoundTheoremUpdater
import scalaz.Scalaz._

object ReplaceElidedSteps extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply(_))
  }

  override def updateElided(step: Step.Elided, stepWithContext: StepWithContext): Step = {
    replaceWithExistingStatementExtraction(step, stepWithContext) orElse
      reprove(step, stepWithContext) getOrElse
      super.updateElided(step, stepWithContext)
  }

  private def replaceWithExistingStatementExtraction(step: Step.Elided, stepWithContext: StepWithContext): Option[Step.ExistingStatementExtraction] = {
    for {
      assertionSteps <- step.substeps.map(_.asOptionalInstanceOf[Step.Assertion]).toList.sequence
      firstAssertion <- assertionSteps.headOption
      mainPremise <- firstAssertion.premises.headOption.map(_.statement)
      _ <- SubstatementExtractor.getPremiseExtractions(mainPremise)(stepWithContext.stepContext)
        .find(_.extractionInferences.toList == assertionSteps.map(_.inference))
    } yield Step.ExistingStatementExtraction(assertionSteps)
  }

  private def reprove(step: Step.Elided, stepWithContext: StepWithContext): Option[Step] = {
    for {
      assertionStep <- step.substeps.lastOption.flatMap(_.asOptionalInstanceOf[Step.Assertion])
      if step.highlightedInference.contains(assertionStep.inference)
      if step.substeps.forall(_.isInstanceOf[Step.Assertion])
      (premiseSteps, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(assertionStep.premises.map(_.statement))(stepWithContext.stepContext)
      if targetSteps.isEmpty
    } yield Step.InferenceWithPremiseDerivations(premiseSteps, assertionStep)
  }
}
