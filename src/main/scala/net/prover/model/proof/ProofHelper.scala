package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model._
import net.prover.model.expressions._

object ProofHelper {
  def findFact(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Step, Inference)] = {
    for {
      (_, inference, extractionOption) <- stepProvingContext.provingContext.facts.find(_._1 == target)
      assertionStep <- Step.Assertion.forInference(inference, Substitutions.empty)
      ExtractionApplication(_, _, extractionSteps, premiseSteps, targetSteps) <- ExtractionHelper.applyExtractions(inference.conclusion, extractionOption.extractionInferences, inference, Substitutions.empty, None, Some(target), _ => (Nil, Nil)).toOption
      if premiseSteps.isEmpty && targetSteps.isEmpty
      finalStep <- Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference)
    } yield (finalStep, inference)
  }

  def findNamingInferences(implicit entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement)] = {
    entryContext.allInferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i).map {
        case (premises, assumption) => (i, premises, assumption)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference)(implicit entryContext: EntryContext): Option[(Seq[Statement], Statement)] = {
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
            Some((initialPremises, assumption))
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
  ): Option[(Step.Assertion, Seq[Step], Seq[Step.Target])] = {
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
      elidedStep <- Step.Elided.ifNecessary(premiseSteps :+ assertionStep, inference)
    } yield (elidedStep, targetSteps)
  }
}
