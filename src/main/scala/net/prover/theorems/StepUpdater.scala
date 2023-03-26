package net.prover.theorems

import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.model.{Inference, Substitutions}
import scalaz.Monad
import scalaz.Scalaz._

abstract class StepUpdater[TParameters, F[_] : Monad] {
  def monad = Monad[F]

  protected def apply(steps: List[Step], parameters: TParameters, boundVariableNames: List[List[String]]): F[List[Step]] = {
    steps.map(apply(_, parameters, boundVariableNames)).sequence
  }
  protected def apply(step: Step, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    step match {
      case step: Step.Target => updateTarget(step, parameters, boundVariableNames)
      case step: Step.Assertion => updateAssertion(step, parameters, boundVariableNames)
      case step: Step.Deduction => updateDeduction(step, parameters, boundVariableNames)
      case step: Step.Generalization => updateGeneralization(step, parameters, boundVariableNames)
      case step: Step.Naming => updateNaming(step, parameters, boundVariableNames)
      case step: Step.SubProof => updateSubProof(step, parameters, boundVariableNames)
      case step: Step.Elided => updateElided(step, parameters, boundVariableNames)
      case step: Step.ExistingStatementExtraction => updateExistingStatementExtraction(step, parameters, boundVariableNames)
    }
  }

  def updateTarget(step: Step.Target, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, parameters, boundVariableNames)
    } yield Step.Target(newStatement)
  }
  def updateAssertion(step: Step.Assertion,  parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newStatement <- updateStatement(step.statement, parameters, boundVariableNames)
      newInference <- updateInference(step.inference, parameters, boundVariableNames)
      newPremises <- step.premises.toList.map(updatePremise(_, parameters, boundVariableNames)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, parameters, boundVariableNames)
    } yield Step.Assertion(newStatement, newInference, newPremises, newSubstitutions)
  }
  def updateDeduction(step: Step.Deduction, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, parameters, boundVariableNames)
      newSubsteps <- apply(step.substeps.toList, parameters, boundVariableNames)
      deductionDefinition <- updateDeductionDefinition(step.deductionDefinition, parameters)
    } yield Step.Deduction(newAssumption, newSubsteps, deductionDefinition)
  }
  def updateGeneralization(step: Step.Generalization, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, parameters, boundVariableNames :+ List(step.variableName))
      generalizationDefinition <- updateGeneralizationDefinition(step.generalizationDefinition, parameters)
    } yield Step.Generalization(step.variableName, newSubsteps, generalizationDefinition)
  }
  def updateNaming(step: Step.Naming, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newAssumption <- updateStatement(step.assumption, parameters, boundVariableNames :+ List(step.variableName))
      newStatement <- updateStatement(step.statement, parameters, boundVariableNames)
      newSubsteps <- apply(step.substeps.toList, parameters, boundVariableNames :+ List(step.variableName))
      newInference <- updateInference(step.inference, parameters, boundVariableNames)
      newPremises <- step.premises.toList.map(updatePremise(_, parameters, boundVariableNames)).sequence
      newSubstitutions <- updateSubstitutions(step.substitutions, parameters, boundVariableNames)
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
  def updateSubProof(step: Step.SubProof, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, parameters, boundVariableNames)
    } yield Step.SubProof(step.name, newSubsteps)
  }
  def updateElided(step: Step.Elided, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, parameters, boundVariableNames)
      newHighlightedInference <- step.highlightedInference.map(updateInference(_, parameters, boundVariableNames)).sequence
    } yield Step.Elided(newSubsteps, newHighlightedInference, step.description)
  }
  def updateExistingStatementExtraction(step: Step.ExistingStatementExtraction, parameters: TParameters, boundVariableNames: List[List[String]]): F[Step] = {
    for {
      newSubsteps <- apply(step.substeps.toList, parameters, boundVariableNames)
    } yield Step.ExistingStatementExtraction(newSubsteps)
  }

  def updateStatement(statement: Statement, parameters: TParameters, boundVariableNames: List[List[String]]): F[Statement]
  def updateInference(inference: Inference.Summary, parameters: TParameters, boundVariableNames: List[List[String]]): F[Inference.Summary]
  def updatePremise(premise: Premise, parameters: TParameters, boundVariableNames: List[List[String]]): F[Premise]
  def updateSubstitutions(substitutions: Substitutions, parameters: TParameters, boundVariableNames: List[List[String]]): F[Substitutions]
  def updateDeductionDefinition(deductionDefinition: DeductionDefinition, parameters: TParameters): F[DeductionDefinition] = monad.point(deductionDefinition)
  def updateGeneralizationDefinition(generalizationDefinition: GeneralizationDefinition, parameters: TParameters): F[GeneralizationDefinition] = monad.point(generalizationDefinition)
}
