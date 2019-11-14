package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions._

object ProofHelper {
  def findFact(target: Statement)(implicit stepContext: StepContext): Option[Step.Assertion] = {
    stepContext.entryContext.inferences
      .filter(_.premises.isEmpty)
      .mapFind { inference =>
        inference.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
          .map { substitutions =>
            Step.Assertion(target, inference.summary, Nil, substitutions)
          }
      }
  }

  def findNamingInferences(entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement)] = {
    entryContext.inferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i, entryContext).map {
        case (premises, assumption) => (i, premises, assumption)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference, entryContext: EntryContext): Option[(Seq[Statement], Statement)] = {
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
    substitutions: Substitutions,
    followUpSteps: Seq[Step] = Nil)(
    implicit stepContext: StepContext
  ): Option[Seq[Step]] = {
    for {
      premiseStatements <- inference.substitutePremises(substitutions)
      conclusion <- inference.substituteConclusion(substitutions)
      (targetSteps, premiseSteps) = premiseStatements.foldLeft((Seq.empty[Step], Seq.empty[Step])) { case ((targetStepsSoFar, premiseStepsSoFar), premiseStatement) =>
        PremiseFinder.findPremiseSteps(premiseStatement) match {
          case Some(newPremiseSteps) =>
            (targetStepsSoFar, premiseStepsSoFar ++ newPremiseSteps)
          case None =>
            val (deconstructedStatements, deconstructionSteps) = PremiseFinder.deconstructStatement(premiseStatement)
            val (deconstructionTargetSteps, deconstructionPremiseSteps) = deconstructedStatements.foldLeft((Seq.empty[Step], Seq.empty[Step])) { case ((otherTargetStepsSoFar, otherPremiseStepsSoFar), deconstructedStatement) =>
              PremiseFinder.findPremiseSteps(deconstructedStatement) match {
                case Some(newPremiseSteps) =>
                  (otherTargetStepsSoFar, otherPremiseStepsSoFar ++ newPremiseSteps)
                case None =>
                  (otherTargetStepsSoFar :+ Step.Target(deconstructedStatement), otherPremiseStepsSoFar)
              }
            }
            (targetStepsSoFar ++ deconstructionTargetSteps, premiseStepsSoFar ++ deconstructionPremiseSteps ++ deconstructionSteps)
        }
      }
      assertionStep = Step.Assertion(
        conclusion,
        inference.summary,
        premiseStatements.map(Premise.Pending),
        substitutions)
      (initialSteps, stepsToElide) = if (InferenceTypes.isTransitivity(inference)) {
        (targetSteps ++ premiseSteps, assertionStep +: followUpSteps)
      } else {
        (targetSteps, (premiseSteps :+ assertionStep) ++ followUpSteps)
      }
    } yield initialSteps ++ Step.Elided.ifNecessary(stepsToElide, inference).toSeq
  }
}
