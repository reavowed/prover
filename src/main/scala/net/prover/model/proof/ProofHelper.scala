package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object ProofHelper {
  def findFact(target: Statement)(implicit provingContext: ProvingContext, stepContext: StepContext): Option[Step.Assertion] = {
    provingContext.entryContext.inferences
      .filter(_.premises.isEmpty)
      .mapFind { inference =>
        inference.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
          .map { substitutions =>
            Step.Assertion(target, inference.summary, Nil, substitutions)
          }
      }
  }

  def findNamingInferences(implicit entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement)] = {
    entryContext.inferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i).map {
        case (premises, assumption) => (i, premises, assumption)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference)(implicit entryContext: EntryContext): Option[(Seq[Statement], Statement)] = {
    (entryContext.scopingDefinitionOption, entryContext.deductionDefinitionOption) match {
      case (Some(scopingDefinition), Some(deductionDefinition)) =>
        inference match {
          case Inference(
            _,
            initialPremises :+
              DefinedStatement(
              Seq(DefinedStatement(
                Seq(assumption: Statement, StatementVariable(deductionConclusionVariableName)),
                `deductionDefinition`
                )),
              `scopingDefinition`),
            StatementVariable(conclusionVariableName)
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
  ): Option[(Step.Assertion, Seq[Step.Assertion], Seq[Step.Target])] = {
    for {
      premiseStatements <- inference.substitutePremises(substitutions)
      conclusion <- inference.substituteConclusion(substitutions)
      (premiseSteps, targetSteps) = PremiseFinder.findPremiseStepsOrTargets(premiseStatements)
      assertionStep = Step.Assertion(
        conclusion,
        inference.summary,
        premiseStatements.map(Premise.Pending),
        substitutions)
    } yield (assertionStep, premiseSteps, targetSteps)
  }

  def getAssertionWithPremisesAndElide(
    inference: Inference,
    substitutions: Substitutions,
    followUpSteps: Seq[Step] = Nil)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Step, Seq[Step.Target])] = {
    for {
      (assertionStep, premiseSteps, targetSteps) <- getAssertionWithPremises(inference, substitutions)
      elidedStep <- Step.Elided.ifNecessary((premiseSteps :+ assertionStep) ++ followUpSteps, inference)
    } yield (elidedStep, targetSteps)
  }
}
