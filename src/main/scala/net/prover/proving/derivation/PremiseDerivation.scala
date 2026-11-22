package net.prover.proving.derivation

import net.prover.model.proof.Step
import net.prover.model.unwrapping.Unwrapper

case class PremiseDerivation(wrappers: Seq[Unwrapper], wrappedDerivation: DerivedStatement) {
  def toProofSteps: Seq[Step] = wrappers.rewrap(wrappedDerivation.toProofSteps)
}
