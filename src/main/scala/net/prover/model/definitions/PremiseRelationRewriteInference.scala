package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{ProofHelper, Step, StepProvingContext}

case class PremiseRelationRewriteInference(inference: Inference, initialPremise: Statement, mainPremise: Statement, conclusion: Statement, extractionOption: ExtractionOption) extends PremiseSimplificationInference {
  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[(Statement, Step, Inference)])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[(Statement, Step, Inference)])] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(premiseToMatch).flatMap(_.confirmTotality)
      (premiseStepsAndInferences, substitutionsAfterInitialPremise) <- existingPremises.mapFind { case (premiseStatement, stepsAndInferences) =>
        initialPremise.calculateSubstitutions(premiseStatement, substitutionsAfterMainPremise).map { s => stepsAndInferences -> s }
      } orElse ProofHelper.findFactBySubstituting(initialPremise, substitutionsAfterMainPremise).map { case (step, statement, inference, substitutions) => (Seq((statement, step, inference)), substitutions) }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality
      (extractionResult, extractionStep) <- ExtractionHelper.getExtractedAssertionStep(inference, substitutions, extractionOption)
    } yield (extractionResult, premiseStepsAndInferences :+ (extractionResult, extractionStep, inference))
  }
}
