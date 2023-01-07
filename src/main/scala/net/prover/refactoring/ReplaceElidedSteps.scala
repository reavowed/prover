package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model.proof.{Step, SubstatementExtractor}
import scalaz.Scalaz._

import scala.util.{Success, Try}

object ReplaceElidedSteps {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => ElidedStepReplacer.updateTheorem(_).get)
  }

  object ElidedStepReplacer extends StepUpdater {
    override def updateElided(step: Step.Elided, stepWithContext: StepWithContext): Option[Try[Step]] = {
      for {
        assertionSteps <- step.substeps.map(_.asOptionalInstanceOf[Step.Assertion]).toList.sequence
        firstAssertion <- assertionSteps.headOption
        mainPremise <- firstAssertion.premises.headOption.map(_.statement)
        _ <- SubstatementExtractor.getPremiseExtractions(mainPremise)(stepWithContext.stepProvingContext)
          .find(_.extractionInferences.toList == assertionSteps.map(_.inference))
      } yield Success(Step.ExistingStatementExtraction(assertionSteps))
    }
  }
}
