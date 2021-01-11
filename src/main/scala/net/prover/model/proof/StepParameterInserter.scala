package net.prover.model.proof

import net.prover.core.expressions.{Statement, Term}
import net.prover.core.substitutions.Substitutions
import net.prover.core.transformers.ParameterInserter

object StepParameterInserter {
  def insertExternalParameters(step: Step, numberOfParametersToInsert: Int): Step = {
    WithContext.insertExternalParameters(step, numberOfParametersToInsert)(StepModificationContext.initial)
  }
  def insertExternalParameters(premise: Premise, numberOfParametersToInsert: Int): Premise = {
    WithContext.insertExternalParameters(premise, numberOfParametersToInsert)(StepModificationContext.initial)
  }

  object WithContext {
    def insertExternalParameters(statement: Statement, numberOfParametersToInsert: Int)(implicit stepModificationContext: StepModificationContext): Statement = {
      ParameterInserter.insertExternalParameters(statement, numberOfParametersToInsert, stepModificationContext.externalDepth)
    }
    def insertExternalParameters(term: Term, numberOfParametersToInsert: Int)(implicit stepModificationContext: StepModificationContext): Term = {
      ParameterInserter.insertExternalParameters(term, numberOfParametersToInsert, stepModificationContext.externalDepth)
    }
    def insertExternalParameters(step: Step, numberOfParametersToInsert: Int)(implicit stepModificationContext: StepModificationContext): Step = step match {
      case targetStep: Step.Target =>
        Step.Target(insertExternalParameters(targetStep.statement, numberOfParametersToInsert))
      case assertionStep: Step.Assertion =>
        Step.Assertion(
          insertExternalParameters(assertionStep.statement, numberOfParametersToInsert),
          assertionStep.inference,
          assertionStep.premises.map(insertExternalParameters(_, numberOfParametersToInsert)),
          insertExternalParameters(assertionStep.substitutions, numberOfParametersToInsert))
      case deductionStep: Step.Deduction =>
        Step.Deduction(
          insertExternalParameters(deductionStep.assumption, numberOfParametersToInsert),
          deductionStep.substeps.map(insertExternalParameters(_, numberOfParametersToInsert)),
          deductionStep.deductionDefinition)
      case generalizationStep: Step.Generalization =>
        Step.Generalization(
          generalizationStep.variableName,
          generalizationStep.substeps.map(insertExternalParameters(_, numberOfParametersToInsert)(stepModificationContext.increaseDepth)),
          generalizationStep.generalizationDefinition)
      case namingStep: Step.Naming =>
        Step.Naming(
          namingStep.variableName,
          insertExternalParameters(namingStep.assumption, numberOfParametersToInsert)(stepModificationContext.increaseDepth),
          insertExternalParameters(namingStep.statement, numberOfParametersToInsert),
          namingStep.substeps.map(insertExternalParameters(_, numberOfParametersToInsert)(stepModificationContext.increaseDepth)),
          namingStep.inference,
          namingStep.premises.map(insertExternalParameters(_, numberOfParametersToInsert)),
          insertExternalParameters(namingStep.substitutions, numberOfParametersToInsert),
          namingStep.generalizationDefinition,
          namingStep.deductionDefinition)
      case elidedStep: Step.Elided =>
        Step.Elided(
          elidedStep.substeps.map(insertExternalParameters(_, numberOfParametersToInsert)),
          elidedStep.highlightedInference,
          elidedStep.description)
      case subproofStep: Step.SubProof =>
        Step.SubProof(
          subproofStep.name,
          subproofStep.substeps.map(insertExternalParameters(_, numberOfParametersToInsert)))
    }
    def insertExternalParameters(premise: Premise, numberOfParametersToInsert: Int)(implicit stepModificationContext: StepModificationContext): Premise = {
      case pending: Premise.Pending =>
        Premise.Pending(ParameterInserter.insertExternalParameters(pending.statement, numberOfParametersToInsert, stepModificationContext.externalDepth))
      case given: Premise.Given =>
        Premise.Given(ParameterInserter.insertExternalParameters(given.statement, numberOfParametersToInsert, stepModificationContext.externalDepth), given.referencedLine)
      case simplification: Premise.Simplification =>
        Premise.Simplification(
          insertExternalParameters(simplification.statement, numberOfParametersToInsert),
          insertExternalParameters(simplification.premise, numberOfParametersToInsert).asInstanceOf[Premise.SingleLinePremise],
          simplification.inference,
          insertExternalParameters(simplification.substitutions, numberOfParametersToInsert),
          simplification.path)
    }
    def insertExternalParameters(substitutions: Substitutions, numberOfParametersToInsert: Int)(implicit stepModificationContext: StepModificationContext): Substitutions = {
      Substitutions(
        substitutions.statements.map(insertExternalParameters(_, numberOfParametersToInsert)),
        substitutions.terms.map(insertExternalParameters(_, numberOfParametersToInsert)))
    }

  }
}
