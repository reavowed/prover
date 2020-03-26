package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{Step, StepProvingContext}

case class ConclusionRelationSimplificationInference(inference: Inference, extractionOption: ExtractionOption) {
  private def findPremises(premises: Seq[Statement])(implicit stepProvingContext: StepProvingContext): (Seq[Step], Seq[Step.Target]) = {
    premises.filter(_ != extractionOption.premises.last).foldLeft((Seq.empty[Step], Seq.empty[Step.Target])) { case ((steps, targets), premise) =>
      stepProvingContext.allPremiseExtractions.find(_._1 == premise).map(_._2.map(_._1)) match {
        case Some(newSteps) =>
          (steps ++ newSteps, targets)
        case None =>
          (steps, targets :+ Step.Target(premise))
      }
    }
  }
  def matchTarget(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Statement, Step)] = {
    for {
      substitutions <- extractionOption.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
      assertionStep <- Step.Assertion.forInference(inference, substitutions)
      ExtractionApplication(_, _, extractionSteps, premiseSteps, targetSteps) <- ExtractionHelper.applyExtractions(assertionStep.statement, extractionOption.extractionInferences, inference, substitutions, None, None, findPremises).toOption
      if targetSteps.isEmpty
      extractionStep = Step.Elided.ifNecessary(assertionStep +: extractionSteps, inference).get
      finalStep = Step.Elided.ifNecessary(premiseSteps :+ extractionStep, inference).get
      substitutedPremises <- inference.substitutePremises(substitutions)
    } yield (substitutedPremises.last, finalStep)
  }
}
