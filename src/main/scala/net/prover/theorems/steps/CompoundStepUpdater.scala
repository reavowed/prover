package net.prover.theorems.steps

import net.prover.entries.{StepWithContext, StepsWithContext}
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext}
import net.prover.model.{Inference, Substitutions}
import scalaz.Monad
import scalaz.Scalaz._

import scala.annotation.tailrec

abstract class CompoundStepUpdater[F[_] : Monad] {
  def apply(stepsWithContext: StepsWithContext): F[List[Step]] = {
    @tailrec def helper(fList: F[List[Step]], stepOption: Option[StepWithContext]): F[List[Step]] = {
      stepOption match {
        case Some(stepWithContext) =>
          val newList = for {
            list <- fList
            step <- apply(stepWithContext)
          } yield list :+ step
          helper(newList, stepWithContext.nextSibling)
        case None =>
          fList
      }
    }
    helper(Monad[F].point(Nil), stepsWithContext.atIndex(0))
  }

  def apply(stepWithContext: StepWithContext): F[Step] = {
    stepWithContext.step match {
      case step: Step.Target => updateTarget(step, stepWithContext)
      case step: Step.Assertion => updateAssertion(step, stepWithContext)
      case step: Step.Deduction => updateDeduction(step, stepWithContext)
      case step: Step.Generalization => updateGeneralization(step, stepWithContext)
      case step: Step.Naming => updateNaming(step, stepWithContext)
      case step: Step.SubProof => updateSubProof(step, stepWithContext)
      case step: Step.Elided => updateElided(step, stepWithContext)
      case step: Step.ExistingStatementExtraction => updateExistingStatementExtraction(step, stepWithContext)
    }
  }

  def updateTarget(step: Step.Target, stepWithContext: StepWithContext): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, stepWithContext.stepContext)
    } yield Step.Target(newStatement)
  }
  def updateAssertion(step: Step.Assertion,  stepWithContext: StepWithContext): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, stepWithContext.stepContext)
      newInference <- updateInference(step.inference, stepWithContext.stepContext)
      newPremises <- step.premises.toList.map(updatePremise(_, stepWithContext.stepProvingContext)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, stepWithContext.stepContext)
    } yield Step.Assertion(newStatement, newInference, newPremises, newSubstitutions)
  }
  def updateDeduction(step: Step.Deduction, stepWithContext: StepWithContext): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, stepWithContext.stepContext)
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      deductionDefinition <- updateDeductionDefinition(step.deductionDefinition)
    } yield Step.Deduction(newAssumption, newSubsteps, deductionDefinition)
  }
  def updateGeneralization(step: Step.Generalization, stepWithContext: StepWithContext): F[Step] = {
    for {
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      generalizationDefinition <- updateGeneralizationDefinition(step.generalizationDefinition)
    } yield Step.Generalization(step.variableName, newSubsteps, generalizationDefinition)
  }
  def updateNaming(step: Step.Naming, stepWithContext: StepWithContext): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, stepWithContext.stepContext.addBoundVariable(step.variableName))
      newStatement <- updateStatement(step.statement, stepWithContext.stepContext)
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      newInference <- updateInference(step.inference, stepWithContext.stepContext)
      newPremises <- step.premises.toList.map(updatePremise(_, stepWithContext.stepProvingContext)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, stepWithContext.stepContext)
      deductionDefinition <- updateDeductionDefinition(step.deductionDefinition)
      generalizationDefinition <- updateGeneralizationDefinition(step.generalizationDefinition)
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
  def updateSubProof(step: Step.SubProof, stepWithContext: StepWithContext): F[Step] = {
    for {
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
    } yield Step.SubProof(step.name, newSubsteps)
  }
  def updateElided(step: Step.Elided, stepWithContext: StepWithContext): F[Step] = {
    for {
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
      newHighlightedInference <- step.highlightedInference.map(updateInference(_, stepWithContext.stepContext)).sequence
    } yield Step.Elided(newSubsteps, newHighlightedInference, step.description)
  }
  def updateExistingStatementExtraction(step: Step.ExistingStatementExtraction, stepWithContext: StepWithContext): F[Step] = {
    for {
      newSubsteps <- apply(stepWithContext.forSubsteps(step))
    } yield Step.ExistingStatementExtraction(newSubsteps)
  }

  def updateStatement(statement: Statement, stepContext: StepContext): F[Statement] = Monad[F].point(statement)
  def updateInference(inference: Inference.Summary, stepContext: StepContext): F[Inference.Summary] = Monad[F].point(inference)
  def updatePremise(premise: Premise, stepProvingContext: StepProvingContext): F[Premise] = Monad[F].point(premise)
  def updateSubstitutions(substitutions: Substitutions, stepContext: StepContext): F[Substitutions] = Monad[F].point(substitutions)
  def updateDeductionDefinition(deductionDefinition: DeductionDefinition): F[DeductionDefinition] = Monad[F].point(deductionDefinition)
  def updateGeneralizationDefinition(generalizationDefinition: GeneralizationDefinition): F[GeneralizationDefinition] = Monad[F].point(generalizationDefinition)
}
