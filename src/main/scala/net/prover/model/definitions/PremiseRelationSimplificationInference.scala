package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepProvingContext}

case class PremiseRelationSimplificationInference(inference: Inference, premise: Statement, conclusion: Statement, extractionInferences: Seq[Inference]) extends PremiseSimplificationInference {
  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[(Step, Inference)])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[(Step, Inference)])] = {
    for {
      substitutions <- premise.calculateSubstitutions(premiseToMatch).flatMap(_.confirmTotality)
      assertionStep <- Step.Assertion.forInference(inference, substitutions)
      ExtractionApplication(extractionResult, _, extractionSteps, _, _) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionInferences, inference, substitutions, None, None, _ => (Nil, Nil)).toOption
      extractionStep = Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get
    } yield (extractionResult, Seq((extractionStep, inference)))
  }
}
