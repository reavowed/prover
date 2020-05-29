package net.prover.model.definitions

import net.prover.model.proof.StepProvingContext
import net.prover.model.proof.SubstatementExtractor.InferenceExtraction

trait PremiseSimplificationInference {
  def inferenceExtraction: InferenceExtraction
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit stepProvingContext: StepProvingContext): Option[KnownStatement]
}
