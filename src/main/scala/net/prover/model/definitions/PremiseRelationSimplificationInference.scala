package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{DerivationStep, StepProvingContext}

case class PremiseRelationSimplificationInference(inference: Inference, premise: Statement, conclusion: Statement, extractionOption: ExtractionOption) extends PremiseSimplificationInference {
  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[DerivationStep])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[DerivationStep])] = {
    for {
      substitutions <- premise.calculateSubstitutions(premiseToMatch).flatMap(_.confirmTotality)
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
    } yield (derivationStep.statement, Seq(derivationStep))
  }
}
