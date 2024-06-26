package net.prover.proving.fromExistingStatement

import net.prover.controllers.models.PossibleConclusionWithPremises
import net.prover.model.Substitutions
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.proving.extraction.PremiseExtraction

object SuggestExistingStatementsForCurrentTarget extends SuggestExistingStatementsBase {
  def getPossibleConclusionWithPremises(
    premiseExtraction: PremiseExtraction,
    step: Step.TargetStep,
    baseSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[PossibleConclusionWithPremises] = {
    PossibleConclusionWithPremises.fromExtractionWithSubstitutions(premiseExtraction, _.calculateSubstitutions(step.statement, baseSubstitutions))
  }
}
