package net.prover.theorems

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.StepContext
import net.prover.theorems.steps.CompoundStepUpdater
import scalaz.Monad
import scalaz.Scalaz._

abstract class CompoundTheoremUpdater[TParameters, F[_] : Monad] extends CompoundStepUpdater[TParameters, F] {
  def apply(theorem: Theorem, parameters: TParameters): F[Theorem] = {
    for {
      newPremises <- theorem.premises.map(updateStatement(_, theorem.initialStepContext, parameters)).toList.sequence
      newConclusion <- updateStatement(theorem.conclusion, theorem.initialStepContext, parameters)
      newProofs <- theorem.proofs.map(apply(_, theorem.initialStepContext, parameters)).toList.sequence
    } yield Theorem(
      theorem.name,
      theorem.variableDefinitions,
      newPremises,
      newConclusion,
      newProofs)
  }
  protected def apply(proof: Proof, initialStepContext: StepContext, parameters: TParameters): F[Proof] = {
    for {
      newSteps <- apply(proof.steps.toList, initialStepContext, parameters)
    } yield proof.copy(steps = newSteps)
  }
}
