package net.prover.theorems

import net.prover.model.definitions.{DeductionDefinition, ExpressionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, StepContext, StepProvingContext}
import net.prover.model.{EntryContext, Inference, Substitutions}
import scalaz.Id.Id

case class ReplaceDefinitions(definitionsToReplace: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext) extends CompoundTheoremUpdater[Id] {
  override def updateStatement(
    statement: Statement,
    stepContext: StepContext
  ): Statement = {
    statement.replaceDefinitions(definitionsToReplace)
  }

  override def updateInference(
    inference: Inference.Summary,
    stepContext: StepContext
  ): Inference.Summary = {
    inference.replaceDefinitions(definitionsToReplace)
  }

  override def updatePremise(
    premise: Premise,
    stepContext: StepProvingContext
  ): Premise = {
    premise.replaceDefinitions(definitionsToReplace)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext
  ): Substitutions = {
    substitutions.replaceDefinitions(definitionsToReplace)
  }

  override def updateDeductionDefinition(
    deductionDefinition: DeductionDefinition
  ): DeductionDefinition = {
    entryContext.deductionDefinitionOption.get
  }

  override def updateGeneralizationDefinition(
    generalizationDefinition: GeneralizationDefinition
  ): GeneralizationDefinition = {
    entryContext.generalizationDefinitionOption.get
  }
}
