package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{PremiseFinder, PremiseStep, ProofHelper, StepProvingContext}

case class PremiseRelationRewriteInference(inference: Inference, initialPremiseOption: Option[Statement], mainPremise: Statement, conclusion: Statement, extractionOption: ExtractionOption, initialSubstitutions: Substitutions.Possible) extends PremiseSimplificationInference {

  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[PremiseStep])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[PremiseStep])] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(premiseToMatch, initialSubstitutions)
      (premiseSteps, _, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          PremiseFinder.findPremiseStepsForStatementBySubstituting(initialPremise, substitutionsAfterMainPremise, existingPremises).headOption
        case None =>
          Some((Nil, (), substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality
      (extractionResult, extractionStep) <- ExtractionHelper.getExtractedAssertionStep(inference, substitutions, extractionOption)
    } yield (extractionResult, premiseSteps :+ PremiseStep(extractionResult, inference, extractionStep))
  }
}
