package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstitutionContext
import net.prover.model.utils.ExpressionUtils.TypeLikeStatement
import net.prover.proving.derivation.SimpleDerivation
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}

case class ConclusionRelationSimplificationInference(inferenceExtraction: InferenceExtraction, typePremiseOption: Option[TypeLikeStatement], derivedPremises: Seq[DerivedPremise]) extends DerivedInference {
  def getConclusionSimplification(target: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[BinaryRelationStatement], SimpleDerivation)] = {
    for {
      substitutions <- conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality(variableDefinitions))
      extractionApplication <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
      if extractionApplication.statement == target
      substitutedTypeStatement <- typePremiseOption.map(_.baseStatement.applySubstitutions(substitutions)).swap
      (simplifiedTargets, derivationSteps) <- derivedPremises.getSubstitutedPremises(substitutions)
      targetRelationStatements <- simplifiedTargets.map(provingContext.findRelation).traverseOption
    } yield (substitutedTypeStatement.toSeq, targetRelationStatements, SimpleDerivation.fromAssertions(derivationSteps) :+ extractionApplication)
  }
}
