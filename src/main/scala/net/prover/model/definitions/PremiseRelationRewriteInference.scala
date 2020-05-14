package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{PremiseFinder, StepProvingContext}
import net.prover.model.{Inference, Substitutions}

case class PremiseRelationRewriteInference(inference: Inference, initialPremiseOption: Option[Statement], mainPremise: Statement, conclusion: Statement, extractionOption: ExtractionOption, initialSubstitutions: Substitutions.Possible) extends PremiseSimplificationInference {

  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit stepProvingContext: StepProvingContext): Option[KnownStatement] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(currentStatement.statement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          PremiseFinder.findDerivationForStatementBySubstituting(initialPremise, substitutionsAfterMainPremise, existingPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((Nil, substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
    } yield currentStatement.extend(premiseDerivation :+ derivationStep)
  }
}
