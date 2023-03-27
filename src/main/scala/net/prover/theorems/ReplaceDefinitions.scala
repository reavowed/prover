package net.prover.theorems

import net.prover.model.definitions.{DeductionDefinition, ExpressionDefinition, GeneralizationDefinition}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, StepContext}
import net.prover.model.{EntryContext, Inference, Substitutions}
import net.prover.util.FunctorTypes.Identity

object ReplaceDefinitions extends CompoundTheoremUpdater[(Map[ExpressionDefinition, ExpressionDefinition], EntryContext), Identity] {
  def apply(theorem: Theorem, replacements: Map[ExpressionDefinition, ExpressionDefinition])(implicit entryContext: EntryContext): Theorem = {
    apply(theorem, (replacements, entryContext))
  }

  override def updateStatement(
    statement: Statement,
    stepContext: StepContext,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): Statement = {
    statement.replaceDefinitions(parameters._1)
  }

  override def updateInference(
    inference: Inference.Summary,
    stepContext: StepContext,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): Identity[Inference.Summary] = {
    inference.replaceDefinitions(parameters._1)
  }

  override def updatePremise(
    premise: Premise,
    stepContext: StepContext,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): Premise = {
    premise.replaceDefinitions(parameters._1)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    stepContext: StepContext,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): Substitutions = {
    substitutions.replaceDefinitions(parameters._1)
  }

  override def updateDeductionDefinition(
    deductionDefinition: DeductionDefinition,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): DeductionDefinition = {
    parameters._2.deductionDefinitionOption.get
  }

  override def updateGeneralizationDefinition(
    generalizationDefinition: GeneralizationDefinition,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): GeneralizationDefinition = {
    parameters._2.generalizationDefinitionOption.get
  }
}
