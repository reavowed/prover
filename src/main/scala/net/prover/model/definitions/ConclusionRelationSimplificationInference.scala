package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{ProofHelper, Step, StepProvingContext}

case class ConclusionRelationSimplificationInference(inference: Inference, extractionOption: ExtractionOption, premiseIndexes: Seq[Int]) {
  private def findPremises(premises: Seq[Statement])(implicit stepProvingContext: StepProvingContext): (Seq[Step], Seq[Step.Target]) = {
    premises.filter(p => !premiseIndexes.contains(extractionOption.premises.indexOf(p))).foldLeft((Seq.empty[Step], Seq.empty[Step.Target])) { case ((steps, targets), premise) =>
      stepProvingContext.allPremiseExtractions.find(_._1 == premise).map(_._2.map(_._1)) match {
        case Some(newSteps) =>
          (steps ++ newSteps, targets)
        case None =>
          (steps, targets :+ Step.Target(premise))
      }
    }
  }
  def matchTarget(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Step)] = {
    for {
      substitutions <- extractionOption.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
      (assertionStep, assertionPremiseSteps, assertionTargetSteps) <- ProofHelper.getAssertionWithPremises(inference, substitutions)
      if assertionTargetSteps.isEmpty
      ExtractionApplication(_, _, extractionSteps, extractionPremiseSteps, extractionTargetSteps) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionOption.extractionInferences, inference, substitutions, None, None, findPremises).toOption
      if extractionTargetSteps.isEmpty
      extractionStep = Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get
      finalStep = Step.Elided.ifNecessary(assertionPremiseSteps ++ extractionPremiseSteps :+ extractionStep, inference).get
      substitutedPremises <- premiseIndexes.map(extractionOption.premises).map(_.applySubstitutions(substitutions)).traverseOption
    } yield (substitutedPremises, finalStep)
  }
}
