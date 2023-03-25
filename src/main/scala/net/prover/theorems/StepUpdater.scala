package net.prover.theorems

import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.model.{Inference, Substitutions}

abstract class StepUpdater[TParameters] {
  protected def apply(steps: Seq[Step], parameters: TParameters, boundVariableNames: List[List[String]]): Seq[Step] = {
    steps.map(apply(_, parameters, boundVariableNames))
  }
  protected def apply(step: Step, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
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

  def updateTarget(step: Step.Target, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newStatement = updateStatement(step.statement, parameters, boundVariableNames)
    Step.Target(newStatement)
  }
  def updateAssertion(step: Step.Assertion,  parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newStatement = updateStatement(step.statement, parameters, boundVariableNames)
    val newInference = updateInference(step.inference, parameters, boundVariableNames)
    val newPremises = step.premises.map(updatePremise(_, parameters, boundVariableNames))
    val newSubstitutions = updateSubstitutions(step.substitutions, parameters, boundVariableNames)
    Step.Assertion(newStatement, newInference, newPremises, newSubstitutions)
  }
  def updateDeduction(step: Step.Deduction, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newAssumption = updateStatement(step.assumption, parameters, boundVariableNames)
    val newSubsteps = apply(step.substeps, parameters, boundVariableNames)
    val deductionDefinition = updateDeductionDefinition(step.deductionDefinition, parameters)
    Step.Deduction(newAssumption, newSubsteps, deductionDefinition)
  }
  def updateGeneralization(step: Step.Generalization, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newSubsteps = apply(step.substeps, parameters, boundVariableNames :+ List(step.variableName))
    val generalizationDefinition = updateGeneralizationDefinition(step.generalizationDefinition, parameters)
    Step.Generalization(step.variableName, newSubsteps, generalizationDefinition)
  }
  def updateNaming(step: Step.Naming, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newAssumption = updateStatement(step.assumption, parameters, boundVariableNames :+ List(step.variableName))
    val newStatement = updateStatement(step.statement, parameters, boundVariableNames)
    val newSubsteps = apply(step.substeps, parameters, boundVariableNames :+ List(step.variableName))
    val newInference = updateInference(step.inference, parameters, boundVariableNames)
    val newPremises = step.premises.map(updatePremise(_, parameters, boundVariableNames))
    val newSubstitutions = updateSubstitutions(step.substitutions, parameters, boundVariableNames)
    val deductionDefinition = updateDeductionDefinition(step.deductionDefinition, parameters)
    val generalizationDefinition = updateGeneralizationDefinition(step.generalizationDefinition, parameters)
    Step.Naming(
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
  def updateSubProof(step: Step.SubProof, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newSubsteps = apply(step.substeps, parameters, boundVariableNames)
    Step.SubProof(step.name, newSubsteps)
  }
  def updateElided(step: Step.Elided, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newSubsteps = apply(step.substeps, parameters, boundVariableNames)
    val newHighlightedInference = step.highlightedInference.map(updateInference(_, parameters, boundVariableNames))
    Step.Elided(newSubsteps, newHighlightedInference, step.description)
  }
  def updateExistingStatementExtraction(step: Step.ExistingStatementExtraction, parameters: TParameters, boundVariableNames: List[List[String]]): Step = {
    val newSubsteps = apply(step.substeps, parameters, boundVariableNames)
    Step.ExistingStatementExtraction(newSubsteps)
  }

  def updateStatement(statement: Statement, parameters: TParameters, boundVariableNames: List[List[String]]): Statement
  def updateInference(inference: Inference.Summary, parameters: TParameters, boundVariableNames: List[List[String]]): Inference.Summary
  def updatePremise(premise: Premise, parameters: TParameters, boundVariableNames: List[List[String]]): Premise
  def updateSubstitutions(substitutions: Substitutions, parameters: TParameters, boundVariableNames: List[List[String]]): Substitutions
  def updateDeductionDefinition(deductionDefinition: DeductionDefinition, parameters: TParameters): DeductionDefinition = deductionDefinition
  def updateGeneralizationDefinition(generalizationDefinition: GeneralizationDefinition, parameters: TParameters): GeneralizationDefinition = generalizationDefinition
}
