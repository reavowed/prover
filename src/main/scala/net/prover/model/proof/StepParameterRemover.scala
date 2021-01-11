package net.prover.model.proof

import net.prover.core.expressions.{Statement, Term}
import net.prover.core.substitutions.Substitutions
import net.prover.model.proof.Premise.SingleLinePremise
import net.prover.model.substitutions.ParameterRemover

object StepParameterRemover {
  def removeExternalParameters(step: Step, numberOfParametersToRemove: Int): Option[Step] = {
    WithContext.removeExternalParameters(step, numberOfParametersToRemove)(StepModificationContext.initial)
  }

  def removeExternalParameters(premise: Premise, numberOfParametersToRemove: Int): Option[Premise] = {
    WithContext.removeExternalParameters(premise, numberOfParametersToRemove)(StepModificationContext.initial)
  }

  object WithContext {
    def removeExternalParameters(statement: Statement, numberOfParametersToRemove: Int)(implicit stepModificationContext: StepModificationContext): Option[Statement] = {
      ParameterRemover.removeExternalParameters(statement, numberOfParametersToRemove)
    }

    def removeExternalParameters(term: Term, numberOfParametersToRemove: Int)(implicit stepModificationContext: StepModificationContext): Option[Term] = {
      ParameterRemover.removeExternalParameters(term, numberOfParametersToRemove)
    }

    def removeExternalParameters(step: Step, numberOfParametersToRemove: Int)(implicit stepModificationContext: StepModificationContext): Option[Step] = step match {
      case targetStep: Step.Target =>
        for {
          newStatement <- removeExternalParameters(targetStep.statement, numberOfParametersToRemove)
        } yield Step.Target(newStatement)
      case assertionStep: Step.Assertion =>
        for {
          newStatement <- removeExternalParameters(assertionStep.statement, numberOfParametersToRemove)
          newPremises <- assertionStep.premises.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
          newSubstitutions <- removeExternalParameters(assertionStep.substitutions, numberOfParametersToRemove)
        } yield Step.Assertion(
          newStatement,
          assertionStep.inference,
          newPremises,
          newSubstitutions)
      case deductionStep: Step.Deduction =>
        for {
          newAssumption <- removeExternalParameters(deductionStep.assumption, numberOfParametersToRemove)
          newSubsteps <- deductionStep.substeps.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
        } yield Step.Deduction(newAssumption, newSubsteps, deductionStep.deductionDefinition)
      case generalizationStep: Step.Generalization =>
        for {
          newSubsteps <- generalizationStep.substeps.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
        } yield Step.Generalization(
          generalizationStep.variableName,
          newSubsteps,
          generalizationStep.generalizationDefinition)
      case namingStep: Step.Naming =>
        for {
          newAssumption <- removeExternalParameters(namingStep.assumption, numberOfParametersToRemove)
          newStatement <- removeExternalParameters(namingStep.statement, numberOfParametersToRemove)
          newSubsteps <- namingStep.substeps.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
          newPremises <- namingStep.premises.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
          newSubstitutions <- removeExternalParameters(namingStep.substitutions, numberOfParametersToRemove)
        } yield Step.Naming(
          namingStep.variableName,
          newAssumption,
          newStatement,
          newSubsteps,
          namingStep.inference,
          newPremises,
          newSubstitutions,
          namingStep.generalizationDefinition,
          namingStep.deductionDefinition)
      case elidedStep: Step.Elided =>
        for {
          newSubsteps <- elidedStep.substeps.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
        } yield Step.Elided(
          newSubsteps,
          elidedStep.highlightedInference,
          elidedStep.description)
      case subproofStep: Step.SubProof =>
        for {
          newSubsteps <- subproofStep.substeps.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
        } yield Step.SubProof(subproofStep.name, newSubsteps)
    }

    def removeExternalParameters(premise: Premise, numberOfParametersToRemove: Int)(implicit stepModificationContext: StepModificationContext): Option[Premise] = {
      case pending: Premise.Pending =>
        for {
          newStatement <- ParameterRemover.removeExternalParameters(pending.statement, numberOfParametersToRemove)
        } yield Premise.Pending(newStatement)
      case given: Premise.Given =>
        for {
          newStatement <- ParameterRemover.removeExternalParameters(given.statement, numberOfParametersToRemove)
        } yield Premise.Given(newStatement, given.referencedLine)
      case simplification: Premise.Simplification =>
        for {
          newStatement <- ParameterRemover.removeExternalParameters(simplification.statement, numberOfParametersToRemove)
          newPremise <- removeExternalParameters(simplification.premise, numberOfParametersToRemove)
          newSubstitutions <- removeExternalParameters(simplification.substitutions, numberOfParametersToRemove)
        } yield Premise.Simplification(
          newStatement,
          newPremise.asInstanceOf[SingleLinePremise],
          simplification.inference,
          newSubstitutions,
          simplification.path)
    }

    def removeExternalParameters(substitutions: Substitutions, numberOfParametersToRemove: Int)(implicit stepModificationContext: StepModificationContext): Option[Substitutions] = {
      for {
        newStatements <- substitutions.statements.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
        newTerms <- substitutions.terms.map(removeExternalParameters(_, numberOfParametersToRemove)).traverseOption
      } yield Substitutions(newStatements, newTerms)
    }
  }

}
