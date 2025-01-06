package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.{StepProvingContext, SubstitutionContext}
import net.prover.model.{ProvingContext, Substitutions}
import net.prover.proving.derivation.SimpleDerivation
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}
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
          Some((SimpleDerivation.empty, substitutionsAfterMainPremise))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      derivationStep <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
    } yield currentStatement.extend(premiseDerivation :+ derivationStep)
  }
  def rewriteTarget(targetStatement: Statement)(implicit stepProvingContext: StepProvingContext): Option[(BinaryRelationStatement, SimpleDerivation)] = {
    for {
      substitutionsAfterConclusion <- inferenceExtraction.conclusion.calculateSubstitutions(targetStatement, initialSubstitutions)
      (premiseDerivation, substitutionsAfterInitialPremise) <- initialPremiseOption match {
        case Some(initialPremise) =>
          DerivationFinder.findDerivationForStatementBySubstituting(initialPremise, substitutionsAfterConclusion, stepProvingContext.knownStatementsFromPremises).headOption.map(_.mapLeft(_.derivation))
        case None =>
          Some((SimpleDerivation.empty, substitutionsAfterConclusion))
      }
      substitutions <- substitutionsAfterInitialPremise.confirmTotality(inferenceExtraction.variableDefinitions)
      derivationStep <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
      substitutedPremise <- mainPremise.applySubstitutions(substitutions)
      premiseRelationStatement <- stepProvingContext.provingContext.findRelation(substitutedPremise)
    } yield (premiseRelationStatement, premiseDerivation :+ derivationStep)
  }
}
