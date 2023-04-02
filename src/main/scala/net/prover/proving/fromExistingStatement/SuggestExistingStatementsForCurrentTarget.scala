package net.prover.proving.fromExistingStatement

import net.prover.controllers.models.PossibleConclusionWithPremises
import net.prover.model.Substitutions
import net.prover.model.proof.SubstatementExtractor.PremiseExtraction
import net.prover.model.proof.{Step, StepContext}

object SuggestExistingStatementsForCurrentTarget extends SuggestExistingStatementsBase {
  def getPossibleConclusionWithPremises(
    premiseExtraction: PremiseExtraction,
    step: Step.Target,
    baseSubstitutions: Substitutions.Possible)(
    implicit stepContext: StepContext
  ): Option[PossibleConclusionWithPremises] = {
    PossibleConclusionWithPremises.fromExtractionWithSubstitutions(premiseExtraction, _.calculateSubstitutions(step.statement, baseSubstitutions))
  }
}
