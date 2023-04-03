package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions._
import net.prover.proving.premiseFinding.DerivationOrTargetFinder

object ProofHelper {
  def findFactBySubstituting(target: Statement, substitutionsSoFar: Substitutions.Possible)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(Step.InferenceApplicationWithoutPremises, Substitutions.Possible)] = {
    provingContext.facts.mapFind { fact =>
      for {
        substitutions <- target.calculateSubstitutions(fact.statement, substitutionsSoFar)
      } yield (fact, substitutions)
    }
  }

  def findNamingInferences(implicit availableEntries: AvailableEntries): Seq[(Inference, Seq[Statement], Statement, GeneralizationDefinition, DeductionDefinition)] = {
    availableEntries.allInferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i).map {
        case (premises, assumption, generalizationDefinition, deductionDefinition) => (i, premises, assumption, generalizationDefinition, deductionDefinition)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference)(implicit availableEntries: AvailableEntries): Option[(Seq[Statement], Statement, GeneralizationDefinition, DeductionDefinition)] = {
    (availableEntries.generalizationDefinitionOption, availableEntries.deductionDefinitionOption) match {
      case (Some(generalizationDefinition), Some(deductionDefinition)) =>
        inference match {
          case Inference(
            _,
            initialPremises :+ generalizationDefinition(_, deductionDefinition(assumption: Statement, StatementVariable(deductionConclusionVariableName, Nil))),
            StatementVariable(conclusionVariableName, Nil)
          ) if deductionConclusionVariableName == conclusionVariableName =>
            Some((initialPremises, assumption, generalizationDefinition, deductionDefinition))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  def getAssertionWithPremises(
    inference: Inference,
    substitutions: Substitutions)(
    implicit stepContext: StepContext
  ): Option[(Step.Assertion, Seq[Step.InferenceApplicationWithoutPremises], Seq[Step.Target])] = {
    for {
      premiseStatements <- inference.substitutePremises(substitutions)
      conclusion <- inference.substituteConclusion(substitutions)
      (premiseSteps, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(premiseStatements)
      assertionStep = Step.Assertion(
        conclusion,
        inference.summary,
        premiseStatements.map(Premise.Pending),
        substitutions.restrictTo(inference.variableDefinitions))
    } yield (assertionStep, premiseSteps, targetSteps)
  }
}
