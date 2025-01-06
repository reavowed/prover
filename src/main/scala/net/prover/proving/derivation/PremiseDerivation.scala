package net.prover.proving.derivation

import net.prover.model.proof.Step
import net.prover.model.unwrapping.Unwrapper

case class PremiseDerivation(wrappers: Seq[Unwrapper], wrappedDerivation: SimpleDerivation) {
  def toProofSteps: Seq[Step] = wrappers.rewrap(wrappedDerivation.toProofSteps)
}
