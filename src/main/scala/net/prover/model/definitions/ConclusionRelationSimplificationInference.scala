package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{PremiseFinder, Step, StepProvingContext}

case class ConclusionRelationSimplificationInference(inference: Inference, extractionOption: ExtractionOption, premisesThatMustBePresent: Seq[Statement], premisesToRecurseOn: Seq[(Statement,BinaryRelation)], conclusionRelation: BinaryRelation) {
  def matchTarget(target: Statement, previousTargets: Seq[Statement])(implicit stepProvingContext: StepProvingContext): Option[(Seq[(Statement, BinaryRelation)], Step)] = {
    for {
      substitutionsAfterConclusion <- extractionOption.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
      (premiseSteps, targetSteps, _, substitutionsAfterPremises) <- PremiseFinder.findPremiseStepsOrTargetsForStatementsBySubstituting(premisesThatMustBePresent, substitutionsAfterConclusion, previousTargets)
      if targetSteps.isEmpty
      substitutions <- substitutionsAfterPremises.confirmTotality
      assertionStep <- Step.Assertion.forInference(inference, substitutions)
      ExtractionApplication(_, _, extractionSteps, _, _) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionOption.extractionInferences, inference, substitutions, None, None, _ => (Nil, Nil)).toOption
      extractionStep = Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get
      finalStep = Step.Elided.ifNecessary(premiseSteps :+ extractionStep, inference).get
      substitutedPremises <- premisesToRecurseOn.map(_.optionMapLeft(_.applySubstitutions(substitutions))).traverseOption
    } yield (substitutedPremises, finalStep)
  }
}
