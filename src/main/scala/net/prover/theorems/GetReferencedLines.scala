package net.prover.theorems

import net.prover.model.proof.{PreviousLineReference, Step}

object GetReferencedLines {
  def apply(step: Step): Set[PreviousLineReference] = {
    GetReferencedPremises(step).flatMap(_.referencedLines).toSet
  }
}
