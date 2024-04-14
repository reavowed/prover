package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.exceptions.InferenceReplacementException
import net.prover.model.Inference
import net.prover.model.proof.Step
import net.prover.proving.extraction.ExtractionApplier
import net.prover.theorems.CompoundTheoremUpdater
import net.prover.util.FunctorTypes._

import scala.util.Try

case class ReplaceInference(oldInference: Inference, newInference: Inference) extends CompoundTheoremUpdater[Try] {
  override def updateAssertion(
    step: Step.AssertionStep,
    stepWithContext: StepWithContext
  ): Try[Step] = {
    import step._
    import stepWithContext.{provingContext, stepContext}
    if (inference == oldInference) {
      val substitutionsOption = (for {
        inferenceExtraction <- stepWithContext.provingContext.inferenceExtractionsByInferenceId(newInference.id)
        substitutionsAfterConclusion <- inferenceExtraction.conclusion.calculateSubstitutions(statement)(stepWithContext).toSeq
        substitutionsAfterPremises <- inferenceExtraction.premises.zipStrict(premises).flatMap(_.foldLeft(Option(substitutionsAfterConclusion)) { case (so, (ep, p)) => so.flatMap(s => ep.calculateSubstitutions(p.statement, s)(stepWithContext)) }).toSeq
        substitutions <- substitutionsAfterPremises.confirmTotality(inferenceExtraction.variableDefinitions).toSeq
      } yield (inferenceExtraction, substitutions)).headOption
      for {
        (inferenceExtraction, substitutions) <- substitutionsOption.failIfUndefined(InferenceReplacementException("Could not find extraction option", stepWithContext))
        extractionStep <- ExtractionApplier.getInferenceExtractionStepWithoutPremises(inferenceExtraction, substitutions).failIfUndefined(InferenceReplacementException("Could not apply extraction", stepWithContext))
      } yield extractionStep
    } else super.updateAssertion(step, stepWithContext)
  }

  override def updateNaming(
    step: Step.NamingStep,
    stepWithContext: StepWithContext
  ): Try[Step] = {
    if (step.inference == oldInference) {
      import step._
      val assertion = Step.AssertionStep(statement, inference, premises, substitutions)
      for {
        updatedStep <- updateAssertion(assertion, stepWithContext)
        updatedAssertion <- updatedStep.asOptionalInstanceOf[Step.AssertionStep].failIfUndefined(InferenceReplacementException("Cannot replace naming with an elided step", stepWithContext))
        updatedSubsteps <- apply(stepWithContext.forSubsteps(step))
      } yield Step.NamingStep(
        variableName,
        assumption,
        statement,
        updatedSubsteps,
        updatedAssertion.inference,
        updatedAssertion.premises,
        updatedAssertion.substitutions,
        generalizationDefinition,
        deductionDefinition
      )
    } else super.updateNaming(step, stepWithContext)
  }

  override def updateElided(
    step: Step.ElidedStep,
    stepWithContext: StepWithContext
  ): Try[Step] = {
    import step._
    if (highlightedInference.contains(oldInference)) {
      for {
        updatedSubsteps <- apply(stepWithContext.forSubsteps(step))
      } yield step.copy(highlightedInference = Some(newInference.summary), substeps = updatedSubsteps)
    } else {
      super.updateElided(step, stepWithContext)
    }
  }
}

object ReplaceInference {
  def apply(
    oldInferenceId: String,
    newInferenceId: String)(
    implicit bookStateManager: BookStateManager
  ): Unit = {
    UpdateTheorems(globalContext => {
      val oldInference = globalContext.definitions.allInferences.find(_.id == oldInferenceId).get
      val newInference = globalContext.definitions.allInferences.find(_.id == newInferenceId).get
      ReplaceInference(oldInference, newInference)(_).get
    })
  }
}
