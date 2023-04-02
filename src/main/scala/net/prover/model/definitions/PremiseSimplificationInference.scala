package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.extraction.SubstatementExtractor.InferenceExtraction

trait PremiseSimplificationInference {
  def inferenceExtraction: InferenceExtraction
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[KnownStatement]
}
