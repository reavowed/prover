package net.prover.theorems

import net.prover.model.proof.{PreviousLineReference, Step}

object GetReferencedLines {
  def apply(step: Step): Set[PreviousLineReference] = {
    GetAllPremises(step).flatMap(_.referencedLines).toSet
  }
}
