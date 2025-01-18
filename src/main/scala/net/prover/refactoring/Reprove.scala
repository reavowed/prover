package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.{StepWithContext, TypedStepWithContext}
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.model.proof.Step.AssertionStep
import net.prover.model.unwrapping.{DeductionUnwrapper, GeneralizationUnwrapper, Unwrapper}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction, ExtractionApplier, InferenceExtraction}
import net.prover.theorems.{CompoundTheoremUpdater, GetReferencedPremises, RecalculateReferences}
import scalaz.Scalaz
import scalaz.Scalaz.Id

import scala.annotation.tailrec

object Reprove extends CompoundTheoremUpdater[Id] {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    UpdateTheorems(_ => apply)
  }

  override def updateAutogenerated(step: Step.Autogenerated, stepWithContext: StepWithContext): Step = {
    step match {
      case step: Step.InferenceWithPremiseDerivationsStep =>
        apply(stepWithContext.withStep(step), step.inference) getOrElse super.updateAutogenerated(step, stepWithContext)
      case _ =>
        super.updateAutogenerated(step, stepWithContext)
    }
  }

  override def updateExistingStatementExtraction(
    step: Step.ExistingStatementExtractionStep,
    stepWithContext: StepWithContext
  ): Step = {
    step.extraction.extractionSteps.head match {
      case AppliedExtractionStep.Assertion(assertionStep) if stepWithContext.stepProvingContext.provingContext.availableEntries.statementDefinitions.exists(_.deconstructionInference.contains(assertionStep.inference)) => {
        Step.InferenceExtractionStep(AppliedInferenceExtraction(assertionStep, AppliedExtraction(step.extraction.extractionSteps.tail)))
      }
      case _ =>
        super.updateExistingStatementExtraction(step, stepWithContext)
    }
  }

  def apply(stepWithContext: TypedStepWithContext[Step.WithSubsteps], inference: Inference): Option[Step] = {
    import stepWithContext.stepProvingContext
    @tailrec def unwrap(stepsWithContext: Seq[StepWithContext], unwrappersSoFar: Seq[Unwrapper] = Nil): (Seq[StepWithContext], Seq[Unwrapper]) = {
      stepsWithContext match {
        case Seq(singleStepWithContext) =>
          singleStepWithContext.step match {
            case step@Step.GeneralizationStep(variableName, _, generalizationDefinition) =>
              unwrap(singleStepWithContext.forSubsteps(step).stepsWithContexts, unwrappersSoFar :+ GeneralizationUnwrapper(variableName, generalizationDefinition, stepWithContext.provingContext.specificationInferenceOption.get))
            case step@Step.DeductionStep(assumption, _, generalizationDefinition) =>
              unwrap(singleStepWithContext.forSubsteps(step).stepsWithContexts, unwrappersSoFar :+ DeductionUnwrapper(assumption, generalizationDefinition, stepWithContext.provingContext.deductionEliminationInferenceOption.get))
            case _ =>
              (stepsWithContext, unwrappersSoFar)
          }
        case _ =>
          (stepsWithContext, unwrappersSoFar)
      }
    }
    @tailrec def unwrapPremises(substeps: Seq[StepWithContext], premises: Seq[Statement], unwrappers: Seq[Unwrapper]): Seq[Statement] = {
      substeps match {
        case substep +: otherSubsteps =>
          GetReferencedPremises(substep).single.map(_.statement) match {
            case Some(premise) if premises.contains(premise) && unwrappers.addToStatement(substep.step.statement) == premise =>
              unwrapPremises(otherSubsteps, premises.map(p => if (p == premise) substep.step.statement else p), unwrappers)
            case _ =>
              premises
          }
        case Nil =>
          premises
      }
    }
    @tailrec def matchPremises(inferenceExtraction: InferenceExtraction, remainingExtractionPremises: Seq[Statement], premiseStatements: Seq[Statement], substitutionsSoFar: Seq[Substitutions.Possible]): Option[Substitutions] = {
      if (substitutionsSoFar.isEmpty)
        None
      else remainingExtractionPremises match {
        case premise +: otherPremises =>
          val newPossibleSubstitutions = premiseStatements.flatMap { premiseStatement =>
            substitutionsSoFar.mapCollect { s =>
              premise.calculateSubstitutions(premiseStatement, s)
            }
          }
          matchPremises(inferenceExtraction, otherPremises, premiseStatements, newPossibleSubstitutions)
        case Nil =>
          substitutionsSoFar.mapFind(_.confirmTotality(inferenceExtraction.variableDefinitions))
      }
    }
    def matchExtraction(inferenceExtraction: InferenceExtraction, premiseStatements: Seq[Statement], target: Statement): Option[Substitutions] = {
      matchPremises(
        inferenceExtraction,
        inferenceExtraction.premises,
        premiseStatements,
        inferenceExtraction.conclusion.calculateSubstitutions(target).toSeq)
    }
    val (substeps, unwrappers) = unwrap(stepWithContext.substeps.stepsWithContexts)
    val premises = unwrapPremises(substeps, GetReferencedPremises(stepWithContext).map(_.statement), unwrappers)
    val unwrappedResult = substeps.last.step.statement
    for {
      (extraction, substitutions) <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
        .mapFind(e => matchExtraction(e, premises, unwrappedResult).map(e -> _))
      (replacementStep, targets) <- ExtractionApplier.getInferenceExtractionStepWithPremises(
        extraction,
        substitutions,
        unwrappers,
        None,
        Some(unwrappedResult)).toOption
      if targets.isEmpty
      (updatedStep, _) <- RecalculateReferences(stepWithContext.withStep(replacementStep)).toOption
    } yield updatedStep
  }
}
