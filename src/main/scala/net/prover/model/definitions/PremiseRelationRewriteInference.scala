package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{PremiseFinder, DerivationStep, ProofHelper, StepProvingContext}

case class PremiseRelationRewriteInference(inference: Inference, initialPremiseOption: Option[Statement], mainPremise: Statement, conclusion: Statement, extractionOption: ExtractionOption, initialSubstitutions: Substitutions.Possible) extends PremiseSimplificationInference {

  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[DerivationStep])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[DerivationStep])] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(premiseToMatch, initialSubstitutions)
      (derivationSteps, _, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          PremiseFinder.findPremiseStepsForStatementBySubstituting(initialPremise, substitutionsAfterMainPremise, existingPremises).headOption
        case None =>
          Some((Nil, (), substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
    } yield (derivationStep.statement, derivationSteps :+ derivationStep)
  }
}
