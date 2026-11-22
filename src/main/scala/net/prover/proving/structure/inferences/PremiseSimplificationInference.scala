package net.prover.proving.structure.inferences

import net.prover.model.ProvingContext
import net.prover.proving.derivation.DerivedStatement
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.extraction.InferenceExtraction

trait PremiseSimplificationInference {
  def inferenceExtraction: InferenceExtraction
  def getPremiseSimplification(currentStatement: DerivedStatement, existingPremises: Seq[DerivedStatement])(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[DerivedStatement]
}
