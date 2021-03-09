package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.expressions.Statement
import net.prover.model.proof.StepProvingContext
import net.prover.model.proof.SubstatementExtractor.InferenceExtraction
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

case class PremiseRelationSimplificationInference(inferenceExtraction: InferenceExtraction, premise: Statement) extends PremiseSimplificationInference {
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit stepProvingContext: StepProvingContext): Option[KnownStatement] = {
    for {
      substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(premise, currentStatement.statement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      derivationStep <- ExtractionHelper.getInferenceExtractionDerivationWithoutPremises(inferenceExtraction, substitutions)
    } yield currentStatement.extend(Seq(derivationStep))
  }
}
