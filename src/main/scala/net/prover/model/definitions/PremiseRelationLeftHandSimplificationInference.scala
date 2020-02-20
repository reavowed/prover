package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model.Inference
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Step, StepProvingContext}

import scala.Ordering.Implicits._

case class PremiseRelationLeftHandSimplificationInference(inference: Inference, premise: Statement, conclusion: Statement, premiseRelation: BinaryRelation, extractionInferences: Seq[Inference]) {
  def matchPremiseToTarget(premiseToMatch: Statement, targetLhs: Term, targetRhs: Term)(implicit stepProvingContext: StepProvingContext): Option[(Statement, Step)] = {
    premiseToMatch match {
      case premiseRelation(premiseToMatchLhs, premiseToMatchRhs)
        if targetLhs.complexity >= premiseToMatchLhs.complexity && targetLhs.definitionUsages.contains(premiseToMatchLhs.definitionUsages) &&
          premiseToMatchRhs.complexity > targetRhs.complexity && premiseToMatchRhs.definitionUsages.contains(targetRhs.definitionUsages)
      =>
        for {
          substitutions <- premise.calculateSubstitutions(premiseToMatch).flatMap(_.confirmTotality)
          assertionStep <- Step.Assertion.forInference(inference, substitutions)
          ExtractionApplication(extractionResult, _, extractionSteps, _, _) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionInferences, inference, substitutions, None, _ => (Nil, Nil)).toOption
        } yield (extractionResult, Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get)
      case _ =>
        None
    }
  }
}
