package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.proof.StepProvingContext

trait PremiseSimplificationInference {
  def inference: Inference
  def getPremiseSimplification(currentStatement: KnownStatement, existingPremises: Seq[KnownStatement])(implicit stepProvingContext: StepProvingContext): Option[KnownStatement]
}
