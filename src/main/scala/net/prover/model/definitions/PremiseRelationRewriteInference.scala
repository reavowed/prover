package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.{ProofHelper, Step, StepProvingContext}

case class PremiseRelationRewriteInference(inference: Inference, initialPremise: Statement, mainPremise: Statement, conclusion: Statement, extractionInferences: Seq[Inference]) extends PremiseSimplificationInference {
  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[(Step, Inference)])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[(Step, Inference)])] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(premiseToMatch).flatMap(_.confirmTotality)
      (premiseStepsAndInferences, substitutionsAfterInitialPremise) <- existingPremises.mapFind { case (premiseStatement, stepsAndInferences) =>
        initialPremise.calculateSubstitutions(premiseStatement, substitutionsAfterMainPremise).map { s => stepsAndInferences -> s }
      } orElse ProofHelper.findFactBySubstituting(initialPremise, substitutionsAfterMainPremise).map { case (step, inference, substitutions) => (Seq((step, inference)), substitutions) }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality
      assertionStep <- Step.Assertion.forInference(inference, substitutions)
      ExtractionApplication(extractionResult, _, extractionSteps, _, _) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionInferences, inference, substitutions, None, None, _ => (Nil, Nil)).toOption
      extractionStep = Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get
    } yield (extractionResult, premiseStepsAndInferences :+ (extractionStep, inference))
  }
}