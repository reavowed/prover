package net.prover.proving.fromExistingStatement

import net.prover.controllers.models.PossibleConclusionWithPremises
import net.prover.model.Substitutions
import net.prover.model.proof.SubstatementExtractor.PremiseExtraction
import net.prover.model.proof.{Step, StepProvingContext}

object SuggestExistingStatementsForNewTarget extends SuggestExistingStatementsBase {
  def getPossibleConclusionWithPremises(
    premiseExtraction: PremiseExtraction,
    step: Step.Target,
    baseSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[PossibleConclusionWithPremises] = {
    Some(PossibleConclusionWithPremises.fromExtraction(premiseExtraction, Some(baseSubstitutions)))
  }
}
