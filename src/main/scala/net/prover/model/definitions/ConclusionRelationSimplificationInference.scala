package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{DerivationStep, Step, StepProvingContext}

case class ConclusionRelationSimplificationInference(inference: Inference, extractionOption: ExtractionOption, premiseDesimplifications: Seq[PremiseDesimplification]) {
  def getConclusionSimplification(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])] = {
    for {
      substitutions <- extractionOption.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
      if derivationStep.statement == target
      (simplifiedTargets, derivationSteps) <- premiseDesimplifications.getSubstitutedPremises(substitutions)
    } yield (simplifiedTargets, derivationSteps :+ derivationStep)
  }
}
