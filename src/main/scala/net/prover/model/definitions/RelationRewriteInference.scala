package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.InferenceExtraction
import net.prover.model.proof.{DerivationStep, StepContext, SubstitutionContext}
import net.prover.model.{ProvingContext, Substitutions}
import net.prover.proving.premiseFinding.DerivationFinder

case class RelationRewriteInference(
    inferenceExtraction: InferenceExtraction,
    initialPremiseOption: Option[Statement],
    mainPremise: Statement,
    premiseRelation: BinaryRelation,
    conclusionRelation: BinaryRelation,
    initialSubstitutions: Substitutions.Possible)
  extends PremiseSimplificationInference
{
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[KnownStatement] = {
    for {
      substitutionsAfterMainPremise <- mainPremise.calculateSubstitutions(currentStatement.statement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          DerivationFinder.findKnownStatementBySubstituting(initialPremise, substitutionsAfterMainPremise, existingPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((Nil, substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
    } yield currentStatement.extend(premiseDerivation :+ derivationStep)
  }
  def rewriteTarget(targetStatement: Statement)(implicit stepContext: StepContext): Option[(BinaryRelationStatement, Seq[DerivationStep])] = {
    for {
      substitutionsAfterConclusion <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          DerivationFinder.findDerivationForStatementBySubstituting(initialPremise, substitutionsAfterConclusion, stepContext.knownStatementsFromPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((Nil, substitutionsAfterConclusion))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
      substitutedPremise <- mainPremise.applySubstitutions(substitutions)
      premiseRelationStatement <- stepContext.provingContext.findRelation(substitutedPremise)
    } yield (premiseRelationStatement, premiseDerivation :+ derivationStep)
  }
}
