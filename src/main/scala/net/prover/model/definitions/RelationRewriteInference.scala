package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{DerivationStep, PremiseFinder, StepProvingContext}
import net.prover.model.{Inference, Substitutions}

case class RelationRewriteInference(
    inference: Inference,
    extractionOption: ExtractionOption,
    initialPremiseOption: Option[Statement],
    mainPremise: Statement,
    premiseRelation: BinaryRelation,
    conclusionRelation: BinaryRelation,
    initialSubstitutions: Substitutions.Possible)
  extends PremiseSimplificationInference
{
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit stepProvingContext: StepProvingContext): Option[KnownStatement] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(currentStatement.statement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          PremiseFinder.findKnownStatementBySubstituting(initialPremise, substitutionsAfterMainPremise, existingPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((Nil, substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
    } yield currentStatement.extend(premiseDerivation :+ derivationStep)
  }
  def rewriteTarget(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[(BinaryRelationStatement, Seq[DerivationStep])] = {
    for {
      substitutionsAfterConclusion <- extractionOption.conclusion.calculateSubstitutions(targetStatement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          PremiseFinder.findDerivationForStatementBySubstituting(initialPremise, substitutionsAfterConclusion, stepProvingContext.knownStatementsFromPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((Nil, substitutionsAfterConclusion))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
      substitutedPremise <- mainPremise.applySubstitutions(substitutions)
      premiseRelationStatement <- stepProvingContext.provingContext.findRelation(substitutedPremise)
    } yield (premiseRelationStatement, premiseDerivation :+ derivationStep)
  }
}
