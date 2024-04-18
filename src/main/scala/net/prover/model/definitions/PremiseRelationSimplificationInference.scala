package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}

case class PremiseRelationSimplificationInference(inferenceExtraction: InferenceExtraction, premise: Statement) extends PremiseSimplificationInference {
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[KnownStatement] = {
    for {
      substitutions <- premise.calculateSubstitutions(currentStatement.statement).flatMap(_.confirmTotality(inferenceExtraction.variableDefinitions))
      appliedSimplification <- ExtractionApplier.applyInferenceExtractionWithoutPremises(inferenceExtraction, substitutions)
    } yield currentStatement.extend(Seq(appliedSimplification))
  }
}
