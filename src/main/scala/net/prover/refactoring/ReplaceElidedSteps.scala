package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model.proof.{Step, SubstatementExtractor}
import net.prover.theorems.CompoundTheoremUpdater
import scalaz.Scalaz._

object ReplaceElidedSteps extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply(_))
  }

  override def updateElided(step: Step.Elided, stepWithContext: StepWithContext): Step = {
    (for {
      assertionSteps <- step.substeps.map(_.asOptionalInstanceOf[Step.Assertion]).toList.sequence
      firstAssertion <- assertionSteps.headOption
      mainPremise <- firstAssertion.premises.headOption.map(_.statement)
      _ <- SubstatementExtractor.getPremiseExtractions(mainPremise)(stepWithContext.stepProvingContext)
        .find(_.extractionInferences.toList == assertionSteps.map(_.inference))
    } yield Step.ExistingStatementExtraction(assertionSteps)) getOrElse super.updateElided(step, stepWithContext)
  }
}
