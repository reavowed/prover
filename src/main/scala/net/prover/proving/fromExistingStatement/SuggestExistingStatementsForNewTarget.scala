package net.prover.proving.fromExistingStatement

import net.prover.controllers.models.PossibleConclusionWithPremises
import net.prover.model.Substitutions
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.proving.extraction.PremiseExtraction

object SuggestExistingStatementsForNewTarget extends SuggestExistingStatementsBase {
  def getPossibleConclusionWithPremises(
    premiseExtraction: PremiseExtraction,
    step: Step.TargetStep,
    baseSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[PossibleConclusionWithPremises] = {
    Some(PossibleConclusionWithPremises.fromExtraction(premiseExtraction, Some(baseSubstitutions)))
  }
}
