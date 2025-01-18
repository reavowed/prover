package net.prover.theorems

import net.prover.entries.StepWithContext
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, SubstitutionContext}
import net.prover.model.{AvailableEntries, Inference, Substitutions}
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import scalaz.Id.Id

case class ReplaceDefinitions(definitionsToReplace: Map[ExpressionDefinition, ExpressionDefinition], availableEntries: AvailableEntries) extends CompoundTheoremUpdater[Id] {
  override def updateStatement(
    statement: Statement,
    substitutionContext: SubstitutionContext
  ): Statement = {
    statement.replaceDefinitions(definitionsToReplace)
  }

  override def updateInference(
    inference: Inference.Summary,
    stepWithContext: StepWithContext
  ): Inference.Summary = {
    inference.replaceDefinitions(definitionsToReplace)
  }

  override def updatePremise(
    premise: Premise,
    stepWithContext: StepWithContext
  ): Premise = {
    premise.replaceDefinitions(definitionsToReplace)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepWithContext: StepWithContext
  ): Substitutions = {
    substitutions.replaceDefinitions(definitionsToReplace)
  }

  override def updateDeductionDefinition(
    deductionDefinition: DeductionDefinition
  ): DeductionDefinition = {
    availableEntries.deductionDefinitionOption.get
  }

  override def updateGeneralizationDefinition(
    generalizationDefinition: GeneralizationDefinition
  ): GeneralizationDefinition = {
    availableEntries.generalizationDefinitionOption.get
  }
}
