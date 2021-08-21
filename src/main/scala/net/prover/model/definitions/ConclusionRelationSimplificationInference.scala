package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.InferenceExtraction
import net.prover.model.proof.{DerivationStep, StepProvingContext}
import net.prover.model.utils.ExpressionUtils.TypeLikeStatement
import net.prover.extensions.ExpressionExtensions._
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

case class ConclusionRelationSimplificationInference(inferenceExtraction: InferenceExtraction, typePremiseOption: Option[TypeLikeStatement], derivedPremises: Seq[DerivedPremise]) extends DerivedInference {
  def getConclusionSimplification(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[BinaryRelationStatement], Seq[DerivationStep])] = {
    for {
      substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(conclusion, target).flatMap(_.confirmTotality(variableDefinitions))
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
      if derivationStep.statement == target
      substitutedTypeStatement <- typePremiseOption.map(s => s.baseStatement.applySubstitutions(substitutions).toOption).swap
      (simplifiedTargets, derivationSteps) <- derivedPremises.getSubstitutedPremises(substitutions)
      targetRelationStatements <- simplifiedTargets.map(stepProvingContext.provingContext.findRelation).traverseOption
    } yield (substitutedTypeStatement.toSeq, targetRelationStatements, derivationSteps :+ derivationStep)
  }
}
