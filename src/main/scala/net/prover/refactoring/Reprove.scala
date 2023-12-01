package net.prover.refactoring

import net.prover.entries.{StepWithContext, TypedStepWithContext}
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.model.unwrapping.{DeductionUnwrapper, GeneralizationUnwrapper, Unwrapper}
import net.prover.proving.extraction.ExtractionApplier
import net.prover.proving.extraction.ExtractionCalculator.InferenceExtraction
import net.prover.theorems.{CompoundTheoremUpdater, GetReferencedPremises, RecalculateReferences}
import scalaz.Scalaz.Id

import scala.annotation.tailrec

object Reprove extends CompoundTheoremUpdater[Id] {
  def apply(stepWithContext: TypedStepWithContext[Step.WithSubsteps], inference: Inference): Option[Step] = {
    import stepWithContext.stepProvingContext
    @tailrec def unwrap(stepsWithContext: Seq[StepWithContext], unwrappersSoFar: Seq[Unwrapper] = Nil): (Seq[StepWithContext], Seq[Unwrapper]) = {
      stepsWithContext match {
        case Seq(singleStepWithContext) =>
          singleStepWithContext.step match {
            case step@Step.Generalization(variableName, _, generalizationDefinition) =>
              unwrap(singleStepWithContext.forSubsteps(step).stepsWithContexts, unwrappersSoFar :+ GeneralizationUnwrapper(variableName, generalizationDefinition, stepWithContext.provingContext.specificationInferenceOption.get._1))
            case step@Step.Deduction(assumption, _, generalizationDefinition) =>
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
        None).toOption
      if targets.isEmpty
      (updatedStep, _) <- RecalculateReferences(stepWithContext.withStep(replacementStep)).toOption
    } yield updatedStep
  }
}
