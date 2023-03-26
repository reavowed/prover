package net.prover.theorems

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.theorems.steps.StepUpdater
import scalaz.Monad
import scalaz.Scalaz._

abstract class TheoremStepUpdater[TParameters, F[_] : Monad] extends StepUpdater[TParameters, F] {
  protected def apply(theorem: Theorem, parameters: TParameters): F[Theorem] = {
    for {
      newPremises <- theorem.premises.map(updateStatement(_, parameters, Nil)).toList.sequence
      newConclusion <- updateStatement(theorem.conclusion, parameters, Nil)
      newProofs <- theorem.proofs.map(apply(_, parameters)).toList.sequence
    } yield Theorem(
      theorem.name,
      theorem.variableDefinitions,
      newPremises,
      newConclusion,
      newProofs)
  }
  protected def apply(proof: Proof, parameters: TParameters): F[Proof] = {
    for {
      newSteps <- apply(proof.steps.toList, parameters, Nil)
    } yield proof.copy(steps = newSteps)
  }
}
