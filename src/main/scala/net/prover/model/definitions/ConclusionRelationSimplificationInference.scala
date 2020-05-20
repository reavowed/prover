package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{DerivationStep, StepProvingContext}
import net.prover.model.utils.ExpressionUtils.{TypeLikeStatement, TypeStatement}

case class ConclusionRelationSimplificationInference(inference: Inference, extractionOption: ExtractionOption, typePremiseOption: Option[TypeLikeStatement], derivedPremises: Seq[DerivedPremise]) {
  def getConclusionSimplification(target: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[BinaryRelationStatement], Seq[DerivationStep])] = {
    for {
      substitutions <- extractionOption.conclusion.calculateSubstitutions(target).flatMap(_.confirmTotality)
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(inference, substitutions, extractionOption)
      if derivationStep.statement == target
      substitutedTypeStatement <- typePremiseOption.map(_.baseStatement.applySubstitutions(substitutions)).swap
      (simplifiedTargets, derivationSteps) <- derivedPremises.getSubstitutedPremises(substitutions)
      targetRelationStatements <- simplifiedTargets.map(stepProvingContext.provingContext.findRelation).traverseOption
    } yield (substitutedTypeStatement.toSeq, targetRelationStatements, derivationSteps :+ derivationStep)
  }
}
