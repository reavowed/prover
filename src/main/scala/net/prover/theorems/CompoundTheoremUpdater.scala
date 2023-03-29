package net.prover.theorems

import net.prover.entries.{ProofWithContext, TheoremWithContext}
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.theorems.steps.CompoundStepUpdater
import scalaz.Monad
import scalaz.Scalaz._

abstract class CompoundTheoremUpdater[F[_] : Monad] extends CompoundStepUpdater[F] {
  def apply(theoremWithContext: TheoremWithContext): F[Theorem] = {
    import theoremWithContext._
    for {
      newPremises <- theorem.premises.map(updateStatement(_, theorem.initialStepContext)).toList.sequence
      newConclusion <- updateStatement(theorem.conclusion, theorem.initialStepContext)
      newProofs <- proofsWithContext.map(apply).toList.sequence
    } yield Theorem(
      theorem.name,
      theorem.variableDefinitions,
      newPremises,
      newConclusion,
      newProofs)
  }
  protected def apply(proofWithContext: ProofWithContext): F[Proof] = {
    import proofWithContext._
    for {
      newSteps <- apply(stepsWithContext)
    } yield proof.copy(steps = newSteps)
  }
}
