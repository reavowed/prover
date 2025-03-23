package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.{ProofWithContext, StepWithContext, TheoremWithContext, TypedStepWithContext}
import net.prover.exceptions.InferenceReplacementException
import net.prover.model._
import net.prover.controllers._
import net.prover.model.definitions.KnownStatement
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext}
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{AppliedInferenceExtraction, ExtractionApplier, InferenceExtraction}
import net.prover.proving.premiseFinding.DerivationOrTargetFinder
import net.prover.proving.rewrite.RewritePremise
import net.prover.theorems.{CompoundTheoremUpdater, GetReferencedPremises, RecalculateReferences}
import net.prover.util.FunctorTypes._

import scala.util.Try

case class ReplaceInference(oldInference: Inference, newInference: Inference) extends CompoundTheoremUpdater[Try] {
  override def updateAssertion(
    step: Step.AssertionStep,
    stepWithContext: StepWithContext
  ): Try[Step] = {
    if (step.inference == oldInference) {
      reproveExtractionAsStep(stepWithContext)
    } else {
      super.updateAssertion(step, stepWithContext)
    }
  }

  override def updateInferenceExtraction(
    step: Step.InferenceExtractionStep,
    stepWithContext: StepWithContext
  ): Try[Step] = {
    if (step.inference == oldInference) {
      reproveExtractionAsStep(stepWithContext)
    } else {
      super.updateInferenceExtraction(step, stepWithContext)
    }
  }

  override def updateInferenceWithPremiseDerivations(
    step: Step.InferenceWithPremiseDerivationsStep,
    stepWithContext: StepWithContext
  ): Try[Step] = {
    if (step.inference == oldInference) {
      reproveExtractionAsStep(stepWithContext)
    } else {
      super.updateInferenceWithPremiseDerivations(step, stepWithContext)
    }
  }

  override def updateRewritePremise(
    rewritePremise: RewritePremise,
    stepContext: StepContext,
    proofWithContext: ProofWithContext
  ): Try[RewritePremise] = {
    rewritePremise match {
      case RewritePremise.ByInference(_, extraction) if extraction.inference == oldInference =>
        for {
          (newExtraction, newPremises) <- reproveExtraction(extraction, stepContext, proofWithContext)
          premise <- RecalculateReferences.updateRewritePremise(
            RewritePremise.ByInference(newPremises, newExtraction),
            stepContext,
            proofWithContext).map(_._1)
        } yield premise
      case RewritePremise.Known(KnownStatement(_, SimpleDerivation(Seq(SimpleDerivationStep.Assertion(step))))) if step.inference == oldInference =>
        val stepWithContext = TypedStepWithContext(step, proofWithContext)(using summon, stepContext.forChild())
        for {
          (newExtraction, newPremises) <- reproveExtraction(stepWithContext)
          premise <- RecalculateReferences.updateRewritePremise(
            RewritePremise.ByInference(newPremises, newExtraction),
            stepContext,
            proofWithContext).map(_._1)
        } yield premise
      case _ =>
        super.updateRewritePremise(rewritePremise, stepContext, proofWithContext)
    }
  }

  override def updateAppliedInferenceExtraction(
    appliedInferenceExtraction: AppliedInferenceExtraction,
    stepContext: StepContext,
    proofWithContext: ProofWithContext
  ): Try[AppliedInferenceExtraction] = {
    if (appliedInferenceExtraction.inference == oldInference) {
      for {
        (newAppliedExtraction, premises) <- reproveExtraction(appliedInferenceExtraction, stepContext, proofWithContext)
        _ <- premises.isEmpty.orException(
          InferenceReplacementException("Could not apply extraction without premises", stepContext, proofWithContext))
        updatedExtraction <- RecalculateReferences.updateAppliedInferenceExtraction(newAppliedExtraction, stepContext, proofWithContext).map(_._1)
      } yield updatedExtraction
    } else
      super.updateAppliedInferenceExtraction(appliedInferenceExtraction, stepContext, proofWithContext)
  }

  private def reproveExtractionAsStep(
    stepWithContext: StepWithContext
  ): Try[Step] = {
    for {
      (appliedExtraction, premises) <- reproveExtraction(stepWithContext)
      step = Step.InferenceWithPremiseDerivationsStep.ifNecessary(premises, appliedExtraction.toStep)
      updatedStep <- RecalculateReferences(stepWithContext.withStep(step)).map(_._1)
    } yield updatedStep
  }

  private def reproveExtraction(
    stepWithContext: StepWithContext
  ): Try[(AppliedInferenceExtraction, Seq[KnownStatement])] = {
    reproveExtraction(
      Some(GetReferencedPremises(stepWithContext)),
      stepWithContext.step.statement)(
      stepWithContext.stepContext,
      stepWithContext.proofWithContext)
  }

  private def reproveExtraction(
    appliedInferenceExtraction: AppliedInferenceExtraction,
    stepContext: StepContext,
    proofWithContext: ProofWithContext
  ): Try[(AppliedInferenceExtraction, Seq[KnownStatement])] = {
    reproveExtraction(
      Some(GetReferencedPremises.removeInternalPremises(GetReferencedPremises(appliedInferenceExtraction), stepContext)),
      appliedInferenceExtraction.statement)(
      stepContext,
      proofWithContext)
  }

  def reproveExtraction(
    premisesOption: Option[Seq[Premise]],
    conclusion: Statement)(
    implicit stepContext: StepContext,
    proofWithContext: ProofWithContext
  ): Try[(AppliedInferenceExtraction, Seq[KnownStatement])] = {
    import proofWithContext.provingContext
    for {
      (inferenceExtraction, substitutions) <- getExtraction(premisesOption, conclusion)
        .failIfUndefined(InferenceReplacementException("Could not find extraction", stepContext, proofWithContext))
      (appliedExtraction, requiredPremises) <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
        .failIfUndefined(InferenceReplacementException("Could not apply extraction", stepContext, proofWithContext))
      (knownStatements, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(
        requiredPremises)(
        new StepProvingContext)
      _ <- targetSteps.isEmpty.orFail(InferenceReplacementException("New extraction required targets", stepContext, proofWithContext))
    } yield (appliedExtraction, knownStatements)
  }

  private def getExtraction(
    premisesOption: Option[Seq[Premise]],
    conclusion: Statement)(
    implicit stepContext: StepContext,
    proofWithContext: ProofWithContext
  ): Option[(InferenceExtraction, Substitutions)] = {
    import proofWithContext.provingContext
    (for {
      inferenceExtraction <- provingContext.inferenceExtractionsByInferenceId(newInference.id)
      substitutionsAfterConclusion <- inferenceExtraction.conclusion
        .calculateSubstitutions(conclusion)
        .toSeq
      possibleFinalSubstitutions = substitutionsAfterConclusion.confirmTotality(inferenceExtraction.variableDefinitions)
      substitutionsAfterPremises <- premisesOption match {
        case Some(premises) if possibleFinalSubstitutions.isEmpty =>
          inferenceExtraction.premises.zipStrict(premises)
            .flatMap(_.foldLeft(Option(substitutionsAfterConclusion)) {
              case (so, (ep, p)) => so.flatMap(s => ep.calculateSubstitutions(p.statement, s))
            }).toSeq
        case _ =>
          Some(substitutionsAfterConclusion)
      }
      substitutions <- substitutionsAfterPremises.confirmTotality(inferenceExtraction.variableDefinitions).toSeq
    } yield (inferenceExtraction, substitutions)).headOption
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
  def apply(
    theoremWithContext: TheoremWithContext,
    oldInferenceId: String,
    newInferenceId: String
  ): Try[Theorem] = {
    val oldInference = theoremWithContext.globalContext.definitions.allInferences.find(_.id == oldInferenceId).get
    val newInference = theoremWithContext.globalContext.definitions.allInferences.find(_.id == newInferenceId).get
    ReplaceInference(oldInference, newInference)(theoremWithContext)
  }
}
