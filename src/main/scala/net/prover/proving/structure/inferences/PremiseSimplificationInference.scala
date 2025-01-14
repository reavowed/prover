package net.prover.proving.structure.inferences

import net.prover.model.ProvingContext
import net.prover.model.definitions.KnownStatement
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.extraction.InferenceExtraction

trait PremiseSimplificationInference {
  def inferenceExtraction: InferenceExtraction
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[KnownStatement]
}
