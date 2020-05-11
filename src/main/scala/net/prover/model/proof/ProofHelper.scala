package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model._
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition, StatementDefinition}
import net.prover.model.expressions._

object ProofHelper {
  def findFact(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[DerivationStep] = {
    for {
      (inference, extractionOption) <- stepProvingContext.provingContext.factsBySerializedStatement.get(target.serialized)
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, Substitutions.empty, extractionOption)
    } yield derivationStep
  }
  def findFactBySubstituting(target: Statement, substitutionsSoFar: Substitutions.Possible)(implicit stepProvingContext: StepProvingContext): Option[(DerivationStep, Substitutions.Possible)] = {
    stepProvingContext.provingContext.facts.mapFind { case (fact, inference, extractionOption) =>
      for {
        substitutions <- target.calculateSubstitutions(fact, substitutionsSoFar)
        derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, Substitutions.empty, extractionOption)
      } yield (derivationStep, substitutions)
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
      premiseStatements <- inference.substitutePremises(substitutions)
      conclusion <- inference.substituteConclusion(substitutions)
      (premiseSteps, targetSteps) = PremiseFinder.findPremiseStepsOrTargets(premiseStatements)
      assertionStep = Step.Assertion(
        conclusion,
        inference.summary,
        premiseStatements.map(Premise.Pending),
        substitutions.copy(
          statements = substitutions.statements.filterKeys(s => inference.requiredSubstitutions.statements.exists(_._1 == s)),
          terms = substitutions.terms.filterKeys(t => inference.requiredSubstitutions.terms.exists(_._1 == t))))
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
