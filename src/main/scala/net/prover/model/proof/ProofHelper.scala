package net.prover.model.proof

import net.prover.model._
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions._
import net.prover.structure.EntryContext
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

object ProofHelper {
  def findFactBySubstituting(target: Statement, substitutionsSoFar: PossibleSubstitutions)(implicit stepProvingContext: StepProvingContext): Option[(DerivationStep, PossibleSubstitutions)] = {
    stepProvingContext.provingContext.facts.mapFind { fact =>
      for {
        substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(target, fact.statement, substitutionsSoFar)
      } yield (fact, substitutions)
    }
  }

  def findNamingInferences(implicit entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement, GeneralizationDefinition, DeductionDefinition)] = {
    entryContext.allInferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i).map {
        case (premises, assumption, generalizationDefinition, deductionDefinition) => (i, premises, assumption, generalizationDefinition, deductionDefinition)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference)(implicit entryContext: EntryContext): Option[(Seq[Statement], Statement, GeneralizationDefinition, DeductionDefinition)] = {
    (entryContext.generalizationDefinitionOption, entryContext.deductionDefinitionOption) match {
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
    implicit stepProvingContext: StepProvingContext
  ): Option[(Step.Assertion, Seq[DerivationStep], Seq[Step.Target])] = {
    for {
      premiseStatements <- inference.substitutePremises(substitutions).toOption
      conclusion <- inference.substituteConclusion(substitutions).toOption
      (premiseSteps, targetSteps) = PremiseFinder.findDerivationsOrTargets(premiseStatements)
      assertionStep = Step.Assertion(
        conclusion,
        inference.summary,
        premiseStatements.map(Premise.Pending),
        substitutions.restrictTo(inference.variableDefinitions))
    } yield (assertionStep, premiseSteps, targetSteps)
  }

  def getAssertionWithPremisesAndElide(
    inference: Inference,
    substitutions: Substitutions)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Step, Seq[Step.Target])] = {
    for {
      (assertionStep, premiseSteps, targetSteps) <- getAssertionWithPremises(inference, substitutions)
      elidedStep <- Step.Elided.ifNecessary(premiseSteps.steps :+ assertionStep, inference)
    } yield (elidedStep, targetSteps)
  }
}
