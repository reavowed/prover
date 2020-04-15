package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions._

object ProofHelper {
  def findFact(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[PremiseStep] = {
    for {
      (_, inference, extractionOption) <- stepProvingContext.provingContext.facts.find(_._1 == target)
      assertionStep <- Step.Assertion.forInference(inference, Substitutions.empty)
      ExtractionApplication(extractionResult, _, extractionSteps, premiseSteps, targetSteps) <- ExtractionHelper.applyExtractions(inference.conclusion, extractionOption.extractionInferences, inference, Substitutions.empty, None, Some(target), _ => (Nil, Nil)).toOption
      if premiseSteps.isEmpty && targetSteps.isEmpty
      finalStep <- Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference)
    } yield PremiseStep(extractionResult, inference, finalStep)
  }
  def findFactBySubstituting(target: Statement, substitutionsSoFar: Substitutions.Possible)(implicit stepProvingContext: StepProvingContext): Option[(PremiseStep, Substitutions.Possible)] = {
    stepProvingContext.provingContext.facts.mapFind { case (fact, inference, extractionOption) =>
      for {
        substitutions <- target.calculateSubstitutions(fact, substitutionsSoFar)
        assertionStep <- Step.Assertion.forInference(inference, Substitutions.empty)
        ExtractionApplication(extractionResult, _, extractionSteps, premiseSteps, targetSteps) <- ExtractionHelper.applyExtractions(inference.conclusion, extractionOption.extractionInferences, inference, Substitutions.empty, None, None, _ => (Nil, Nil)).toOption
        if premiseSteps.isEmpty && targetSteps.isEmpty
        finalStep <- Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference)
      } yield (PremiseStep(extractionResult, inference, finalStep), substitutions)
    }
  }

  def findNamingInferences(implicit entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement, StatementDefinition, StatementDefinition)] = {
    entryContext.allInferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i).map {
        case (premises, assumption, generalizationDefinition, deductionDefinition) => (i, premises, assumption, generalizationDefinition, deductionDefinition)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference)(implicit entryContext: EntryContext): Option[(Seq[Statement], Statement, StatementDefinition, StatementDefinition)] = {
    (entryContext.generalizationDefinitionOption, entryContext.deductionDefinitionOption) match {
      case (Some(generalizationDefinition), Some(deductionDefinition)) =>
        inference match {
          case Inference(
            _,
            initialPremises :+
              DefinedStatement(
              Seq(DefinedStatement(
                Seq(assumption: Statement, StatementVariable(deductionConclusionVariableName, Nil)),
                `deductionDefinition`
                )),
              `generalizationDefinition`),
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
  ): Option[(Step.Assertion, Seq[PremiseStep], Seq[Step.Target])] = {
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
