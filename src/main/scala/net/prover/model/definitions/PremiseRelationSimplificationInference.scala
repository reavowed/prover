package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.extraction.ExtractionApplier
import net.prover.proving.extraction.ExtractionCalculator.InferenceExtraction

case class PremiseRelationSimplificationInference(inferenceExtraction: InferenceExtraction, premise: Statement) extends PremiseSimplificationInference {
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[KnownStatement] = {
    for {
      substitutions <- premise.calculateSubstitutions(currentStatement.statement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      derivationStep <- ExtractionApplier.getInferenceExtractionStepWithoutPremises(inferenceExtraction, substitutions)
    } yield currentStatement.extend(Seq(derivationStep))
  }
}
