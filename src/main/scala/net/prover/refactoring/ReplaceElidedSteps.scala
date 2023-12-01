package net.prover.refactoring

import net.prover.books.management.BookStateManager
import net.prover.entries.StepWithContext
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.model.unwrapping.{DeductionUnwrapper, GeneralizationUnwrapper, Unwrapper}
import net.prover.proving.extraction.ExtractionCalculator.InferenceExtraction
import net.prover.proving.extraction.{ExtractionApplier, ExtractionCalculator}
import net.prover.theorems.{CompoundTheoremUpdater, GetReferencedPremises, RecalculateReferences}
import scalaz.Scalaz._

import scala.annotation.tailrec

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
    import stepWithContext.stepProvingContext
    @tailrec def unwrap(stepsWithContext: Seq[StepWithContext], unwrappersSoFar: Seq[Unwrapper] = Nil): (Seq[StepWithContext], Seq[Unwrapper]) = {
      stepsWithContext match {
        case Seq(singleStepWithContext) =>
          singleStepWithContext.step match {
            case step @ Step.Generalization(variableName, _, generalizationDefinition) =>
              unwrap(singleStepWithContext.forSubsteps(step).stepsWithContexts, unwrappersSoFar :+ GeneralizationUnwrapper(variableName, generalizationDefinition, stepWithContext.provingContext.specificationInferenceOption.get._1))
            case step @ Step.Deduction(assumption, _, generalizationDefinition) =>
              unwrap(singleStepWithContext.forSubsteps(step).stepsWithContexts, unwrappersSoFar :+ DeductionUnwrapper(assumption, generalizationDefinition, stepWithContext.provingContext.deductionEliminationInferenceOption.get._1))
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
            case Some(premise) if premises.contains(premise) =>
              unwrapPremises(otherSubsteps, premises.map(p => if (p == premise) substep.step.statement else p), unwrappers)
            case _ =>
              premises
          }
        case Nil =>
          premises
      }
    }
    @tailrec def matchPremises(inference: Inference, remainingExtractionPremises: Seq[Statement], premiseStatements: Seq[Statement], substitutionsSoFar: Seq[Substitutions.Possible]): Option[Substitutions] = {
      if (substitutionsSoFar.isEmpty)
        None
      else remainingExtractionPremises match {
        case premise +: otherPremises =>
          val newPossibleSubstitutions = premiseStatements.flatMap { premiseStatement =>
            substitutionsSoFar.mapCollect { s =>
              premise.calculateSubstitutions(premiseStatement, s)
            }
          }
          matchPremises(inference, otherPremises, premiseStatements, newPossibleSubstitutions)
        case Nil =>
          substitutionsSoFar.mapFind(_.confirmTotality(inference.variableDefinitions))
      }
    }
    def matchExtraction(inferenceExtraction: InferenceExtraction, premiseStatements: Seq[Statement], target: Statement): Option[Substitutions] = {
      matchPremises(
        inferenceExtraction.inference,
        inferenceExtraction.premises,
        premiseStatements,
        inferenceExtraction.conclusion.calculateSubstitutions(target).toSeq)
    }

    for {
      inference <- baseStep.highlightedInference
      (substeps, unwrappers) = unwrap(stepWithContext.forSubsteps(baseStep).stepsWithContexts)
      premises = unwrapPremises(substeps, GetReferencedPremises(stepWithContext).map(_.statement), unwrappers)
      unwrappedResult = substeps.last.step.statement
      (extraction, substitutions) <- stepProvingContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
        .mapFind(e => matchExtraction(e, premises, unwrappedResult).map(e -> _))
      (replacementStep, targets) <- ExtractionApplier.getInferenceExtractionStepWithPremises(
        extraction,
        substitutions,
        unwrappers,
        None,
        None).toOption
      if targets.isEmpty
      (updatedStep, _) <- RecalculateReferences(stepWithContext.withStep(replacementStep)).toOption
    } yield updatedStep
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
