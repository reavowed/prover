package net.prover.proving.fromExistingStatement

import net.prover.controllers.models.PossibleConclusionWithPremises
import net.prover.model.Substitutions
import net.prover.model.proof.{Step, StepContext}
import net.prover.proving.extraction.ExtractionCalculator.PremiseExtraction

object SuggestExistingStatementsForNewTarget extends SuggestExistingStatementsBase {
  def getPossibleConclusionWithPremises(
    premiseExtraction: PremiseExtraction,
    step: Step.Target,
    baseSubstitutions: Substitutions.Possible)(
    implicit stepContext: StepContext
  ): Option[PossibleConclusionWithPremises] = {
    Some(PossibleConclusionWithPremises.fromExtraction(premiseExtraction, Some(baseSubstitutions)))
  }
}
