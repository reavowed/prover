package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{PremiseStep, Step, StepProvingContext}

case class ConclusionRelationSimplificationInference(inference: Inference, extractionOption: ExtractionOption, premiseDesimplifications: Seq[PremiseDesimplification]) {
  def getConclusionSimplification(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[PremiseStep])] = {
    for {
      substitutions <- extractionOption.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
      (extractionResult, extractionStep) <- ExtractionHelper.getExtractedAssertionStep(inference, substitutions, extractionOption)
      if extractionResult == target
      (simplifiedTargets, premiseSteps) <- premiseDesimplifications.getSubstitutedPremises(substitutions)
    } yield (simplifiedTargets, premiseSteps :+ PremiseStep(extractionResult, inference, extractionStep))
  }
}
