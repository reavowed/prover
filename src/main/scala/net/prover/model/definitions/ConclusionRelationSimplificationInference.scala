package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{Step, StepProvingContext}

case class ConclusionRelationSimplificationInference(inference: Inference, extractionOption: ExtractionOption) {
  def getConclusionSimplification(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Step)] = {
    for {
      substitutions <- target.calculateSubstitutions(extractionOption.conclusion).flatMap(_.confirmTotality)
      assertionStep <- Step.Assertion.forInference(inference, substitutions)
      ExtractionApplication(extractionResult, _, extractionSteps, _, _) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionOption.extractionInferences, inference, substitutions, None, None, _ => (Nil, Nil)).toOption
      if extractionResult == target
      simplifiedTargets <- extractionOption.premises.map(_.applySubstitutions(substitutions)).traverseOption
      extractionStep = Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get
    } yield (simplifiedTargets, extractionStep)
  }
}
