package net.prover.theorems

import net.prover.model.definitions.{DeductionDefinition, ExpressionDefinition, GeneralizationDefinition}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.Premise
import net.prover.model.{EntryContext, Inference, Substitutions}
import net.prover.util.FunctorTypes.Identity

object ReplaceDefinitions extends TheoremStepUpdater[(Map[ExpressionDefinition, ExpressionDefinition], EntryContext), Identity] {
  def apply(theorem: Theorem, replacements: Map[ExpressionDefinition, ExpressionDefinition])(implicit entryContext: EntryContext): Theorem = {
    apply(theorem, (replacements, entryContext))
  }

  override def updateStatement(
    statement: Statement,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext),
    boundVariableNames: List[List[String]]
  ): Identity[Statement] = {
    statement.replaceDefinitions(parameters._1)
  }

  override def updateInference(
    inference: Inference.Summary,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext),
    boundVariableNames: List[List[String]]
  ): Identity[Inference.Summary] = {
    inference.replaceDefinitions(parameters._1)
  }

  override def updatePremise(
    premise: Premise,
    parameters: (Map[ExpressionDefinition,
    ExpressionDefinition], EntryContext), boundVariableNames: List[List[String]]
  ): Identity[Premise] = {
    premise.replaceDefinitions(parameters._1)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext),
    boundVariableNames: List[List[String]]
  ): Identity[Substitutions] = {
    substitutions.replaceDefinitions(parameters._1)
  }

  override def updateDeductionDefinition(
    deductionDefinition: DeductionDefinition,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): Identity[DeductionDefinition] = {
    parameters._2.deductionDefinitionOption.get
  }

  override def updateGeneralizationDefinition(
    generalizationDefinition: GeneralizationDefinition,
    parameters: (Map[ExpressionDefinition, ExpressionDefinition], EntryContext)
  ): Identity[GeneralizationDefinition] = {
    parameters._2.generalizationDefinitionOption.get
  }
}
