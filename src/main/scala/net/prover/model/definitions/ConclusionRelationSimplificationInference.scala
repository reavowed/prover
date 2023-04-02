package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.InferenceExtraction
import net.prover.model.proof.{DerivationStep, SubstitutionContext}
import net.prover.model.utils.ExpressionUtils.TypeLikeStatement

case class ConclusionRelationSimplificationInference(inferenceExtraction: InferenceExtraction, typePremiseOption: Option[TypeLikeStatement], derivedPremises: Seq[DerivedPremise]) extends DerivedInference {
  def getConclusionSimplification(target: Statement)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[BinaryRelationStatement], Seq[DerivationStep])] = {
    for {
      substitutions <- conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality(variableDefinitions))
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
      if derivationStep.statement == target
      substitutedTypeStatement <- typePremiseOption.map(_.baseStatement.applySubstitutions(substitutions)).swap
      (simplifiedTargets, derivationSteps) <- derivedPremises.getSubstitutedPremises(substitutions)
      targetRelationStatements <- simplifiedTargets.map(provingContext.findRelation).traverseOption
    } yield (substitutedTypeStatement.toSeq, targetRelationStatements, derivationSteps :+ derivationStep)
  }
}
