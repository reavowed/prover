package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model.proof.Step
import net.prover.model.unwrapping.{DeductionUnwrapper, GeneralizationUnwrapper, Unwrapper}
import net.prover.proving.extraction.SubstatementExtractor
import net.prover.proving.premiseFinding.DerivationOrTargetFinder
import net.prover.theorems.{CompoundTheoremUpdater, GetAllPremises, RecalculateReferences}
import scalaz.Scalaz._

import scala.annotation.tailrec

object ReplaceElidedSteps extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply(_))
  }

  override def updateElided(step: Step.Elided, stepWithContext: StepWithContext): Step = {
    replaceWithWrapped(step, stepWithContext) orElse
    replaceWithExistingStatementExtraction(step, stepWithContext) orElse
      reprove(step, stepWithContext) getOrElse
      super.updateElided(step, stepWithContext)
  }

  private def replaceWithWrapped(baseStep: Step.Elided, stepWithContext: StepWithContext): Option[Step] = {
    import stepWithContext.stepContext
    @tailrec def helper(unwrappers: Seq[Unwrapper], steps: Seq[Step]): Option[Step] = {
      steps match {
        case Seq(Step.Generalization(variableName, substeps, generalizationDefinition)) =>
          helper(unwrappers :+ GeneralizationUnwrapper(variableName, generalizationDefinition, stepWithContext.provingContext.specificationInferenceOption.get._1), substeps)
        case Seq(Step.Deduction(assumption, substeps, generalizationDefinition)) =>
          helper(unwrappers :+ DeductionUnwrapper(assumption, generalizationDefinition, stepWithContext.provingContext.deductionEliminationInferenceOption.get._1), substeps)
        case steps if unwrappers.nonEmpty =>
          for {
            assertionStep <- steps.last.asOptionalInstanceOf[Step.Assertion]
            premises = assertionStep.premises.map(_.statement).filter(s => !stepContext.allPremises.exists(p => p.statement == s))
            (wrappedStep, _) = unwrappers.addNecessaryExtractions(assertionStep, premises)
            updatedStep = RecalculateReferences(stepWithContext.copy(step = wrappedStep))._1
            if updatedStep.asOptionalInstanceOf[Step.WithSubsteps].map(_.substeps).contains(baseStep.substeps)
          } yield updatedStep
        case _ =>
          None
      }
    }
    helper(Nil, baseStep.substeps)
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

  private def reprove(step: Step.Elided, stepWithContext: StepWithContext): Option[Step.InferenceWithPremiseDerivations] = {
    for {
      assertionStep <- step.substeps.lastOption.flatMap(_.asOptionalInstanceOf[Step.Assertion])
      if step.highlightedInference.contains(assertionStep.inference)
      if step.substeps.forall(_.isInstanceOf[Step.Assertion])
      (premiseSteps, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(assertionStep.premises.map(_.statement))(stepWithContext.stepContext)
      if targetSteps.isEmpty
    } yield Step.InferenceWithPremiseDerivations(premiseSteps, assertionStep)
  }
}
