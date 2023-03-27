package net.prover.theorems.steps

import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext}
import net.prover.model.{Inference, Substitutions}
import scalaz.Monad
import scalaz.Scalaz._

abstract class CompoundStepUpdater[TParameters, F[_] : Monad] {
  def apply(steps: List[Step], outerContext: StepContext, parameters: TParameters): F[List[Step]] = {
    steps.zipWithIndex.foldLeft(Monad[F].point((outerContext, List.empty[Step]))) { case (fStepContextAndSteps, (step, index)) =>
      for {
        stepContextAndSteps <- fStepContextAndSteps
        (currentStepContext, steps) = stepContextAndSteps
        innerStepContext = currentStepContext.atIndex(index)
        newStep <- apply(step, innerStepContext, parameters)
      } yield (currentStepContext.addStep(newStep, innerStepContext.stepReference), steps :+ newStep)
    }.map(_._2)
  }
  
  def apply(step: Step, stepContext: StepContext, parameters: TParameters): F[Step] = {
    step match {
      case step: Step.Target => updateTarget(step, stepContext, parameters)
      case step: Step.Assertion => updateAssertion(step, stepContext, parameters)
      case step: Step.Deduction => updateDeduction(step, stepContext, parameters)
      case step: Step.Generalization => updateGeneralization(step, stepContext, parameters)
      case step: Step.Naming => updateNaming(step, stepContext, parameters)
      case step: Step.SubProof => updateSubProof(step, stepContext, parameters)
      case step: Step.Elided => updateElided(step, stepContext, parameters)
      case step: Step.ExistingStatementExtraction => updateExistingStatementExtraction(step, stepContext, parameters)
    }
  }

  def updateTarget(step: Step.Target, stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, stepContext, parameters)
    } yield Step.Target(newStatement)
  }
  def updateAssertion(step: Step.Assertion,  stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, stepContext, parameters)
      newInference <- updateInference(step.inference, stepContext, parameters)
      newPremises <- step.premises.toList.map(updatePremise(_, stepContext, parameters)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, stepContext, parameters)
    } yield Step.Assertion(newStatement, newInference, newPremises, newSubstitutions)
  }
  def updateDeduction(step: Step.Deduction, stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, stepContext, parameters)
      newSubsteps <- apply(step.substeps.toList, stepContext.addStatement(newAssumption, "a"), parameters)
      deductionDefinition <- updateDeductionDefinition(step.deductionDefinition, parameters)
    } yield Step.Deduction(newAssumption, newSubsteps, deductionDefinition)
  }
  def updateGeneralization(step: Step.Generalization, stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, stepContext.addBoundVariable(step.variableName), parameters)
      generalizationDefinition <- updateGeneralizationDefinition(step.generalizationDefinition, parameters)
    } yield Step.Generalization(step.variableName, newSubsteps, generalizationDefinition)
  }
  def updateNaming(step: Step.Naming, stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, stepContext.addBoundVariable(step.variableName), parameters)
      newStatement <- updateStatement(step.statement, stepContext, parameters)
      newSubsteps <- apply(step.substeps.toList, stepContext.addBoundVariable(step.variableName).addStatement(newAssumption, "a"), parameters)
      newInference <- updateInference(step.inference, stepContext, parameters)
      newPremises <- step.premises.toList.map(updatePremise(_, stepContext, parameters)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, stepContext, parameters)
      deductionDefinition <- updateDeductionDefinition(step.deductionDefinition, parameters)
      generalizationDefinition <- updateGeneralizationDefinition(step.generalizationDefinition, parameters)
    } yield Step.Naming(
      step.variableName,
      newAssumption,
      newStatement,
      newSubsteps,
      newInference,
      newPremises,
      newSubstitutions,
      generalizationDefinition,
      deductionDefinition)
  }
  def updateSubProof(step: Step.SubProof, stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, stepContext, parameters)
    } yield Step.SubProof(step.name, newSubsteps)
  }
  def updateElided(step: Step.Elided, stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, stepContext, parameters)
      newHighlightedInference <- step.highlightedInference.map(updateInference(_, stepContext, parameters)).sequence
    } yield Step.Elided(newSubsteps, newHighlightedInference, step.description)
  }
  def updateExistingStatementExtraction(step: Step.ExistingStatementExtraction, stepContext: StepContext, parameters: TParameters): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, stepContext, parameters)
    } yield Step.ExistingStatementExtraction(newSubsteps)
  }

  def updateStatement(statement: Statement, stepContext: StepContext, parameters: TParameters): F[Statement]
  def updateInference(inference: Inference.Summary, stepContext: StepContext, parameters: TParameters): F[Inference.Summary]
  def updatePremise(premise: Premise, stepContext: StepContext, parameters: TParameters): F[Premise]
  def updateSubstitutions(substitutions: Substitutions, stepContext: StepContext, parameters: TParameters): F[Substitutions]
  def updateDeductionDefinition(deductionDefinition: DeductionDefinition, parameters: TParameters): F[DeductionDefinition] = Monad[F].point(deductionDefinition)
  def updateGeneralizationDefinition(generalizationDefinition: GeneralizationDefinition, parameters: TParameters): F[GeneralizationDefinition] = Monad[F].point(generalizationDefinition)
}
