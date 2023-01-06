package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.controllers.ExtractionHelper
import net.prover.entries.StepWithContext
import net.prover.exceptions.InferenceReplacementException
import net.prover.model.Inference
import net.prover.model.proof.Step
import scalaz.Scalaz._

import scala.util.Try

object ReplaceInference {
  def apply(
    oldInferenceId: String,
    newInferenceId: String)(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    UpdateTheorems(definitions => {
      val oldInference = definitions.allInferences.find(_.id == oldInferenceId).get
      val newInference = definitions.allInferences.find(_.id == newInferenceId).get
      val replacer = StepInferenceReplacer(oldInference, newInference)
      replacer.updateTheorem(_).get
    })
  }

  case class StepInferenceReplacer(oldInference: Inference, newInference: Inference) extends StepUpdater {
    override def updateAssertion(step: Step.Assertion, stepWithContext: StepWithContext): Option[Try[Step]] = {
      import step._
      import stepWithContext._
      implicit val spc = stepProvingContext
      if (inference == oldInference) {
        val substitutionsOption = (for {
          inferenceExtraction <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId(newInference.id)
          substitutionsAfterConclusion <- inferenceExtraction.conclusion.calculateSubstitutions(statement).toSeq
          substitutionsAfterPremises <- inferenceExtraction.premises.zipStrict(premises).flatMap(_.foldLeft(Option(substitutionsAfterConclusion)) { case (so, (ep, p)) => so.flatMap(s => ep.calculateSubstitutions(p.statement, s)) }).toSeq
          substitutions <- substitutionsAfterPremises.confirmTotality(inferenceExtraction.variableDefinitions).toSeq
        } yield (inferenceExtraction, substitutions)).headOption
        (for {
          (inferenceExtraction, substitutions) <- substitutionsOption.failIfUndefined(InferenceReplacementException("Could not find extraction option", stepWithContext))
          extractionStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions).failIfUndefined(InferenceReplacementException("Could not apply extraction", stepWithContext))
        } yield extractionStep.step).some
      } else None
    }

    override def updateNaming(step: Step.Naming, stepWithContext: StepWithContext): Option[Try[Step]] = {
      if (step.inference == oldInference) {
        import step._
        val assertion = Step.Assertion(statement, inference, premises, substitutions)
        (for {
          updatedStep <- updateStep(stepWithContext.copy(step = assertion))
          updatedAssertion <- updatedStep.asOptionalInstanceOf[Step.Assertion].failIfUndefined(InferenceReplacementException("Cannot replace naming with an elided step", stepWithContext))
          updatedSubsteps <- updateSteps(stepWithContext.forSubsteps(step))
        } yield Step.Naming(
          variableName,
          assumption,
          statement,
          updatedSubsteps,
          updatedAssertion.inference,
          updatedAssertion.premises,
          updatedAssertion.substitutions,
          generalizationDefinition,
          deductionDefinition
        )).some
      } else None
    }

    override def updateElided(step: Step.Elided, stepWithContext: StepWithContext): Option[Try[Step]] = {
      import step._
      if (highlightedInference.contains(oldInference)) {
        (for {
          updatedSubsteps <- updateSteps(stepWithContext.forSubsteps(step))
        } yield step.copy(highlightedInference = Some(newInference.summary), substeps = updatedSubsteps)).some
      } else {
        None
      }
    }
  }
}
