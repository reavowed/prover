package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{Step, StepProvingContext}

case class PremiseRelationSimplificationInference(inference: Inference, premise: Statement, conclusion: Statement, extractionOption: ExtractionOption) extends PremiseSimplificationInference {
  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[(Step, Inference)])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[(Step, Inference)])] = {
    for {
      substitutions <- premise.calculateSubstitutions(premiseToMatch).flatMap(_.confirmTotality)
      (extractionResult, extractionStep) <- ExtractionHelper.getExtractedAssertionStep(inference, substitutions, extractionOption)
    } yield (extractionResult, Seq((extractionStep, inference)))
  }
}
