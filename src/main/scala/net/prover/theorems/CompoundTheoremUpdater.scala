package net.prover.theorems

import net.prover.entries.{ProofWithContext, TheoremWithContext}
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.SubstitutionContext
import net.prover.theorems.steps.CompoundStepUpdater
import scalaz.Monad
import scalaz.Scalaz._

abstract class CompoundTheoremUpdater[F[_] : Monad] extends CompoundStepUpdater[F] {
  def apply(theoremWithContext: TheoremWithContext): F[Theorem] = {
    import theoremWithContext._
    for {
      newPremises <- theorem.premises.map(updateStatement(_, SubstitutionContext.outsideProof)).toList.sequence
      newConclusion <- updateStatement(theorem.conclusion, SubstitutionContext.outsideProof)
      newProofs <- proofsWithContext.map(apply).toList.sequence
      newTheorem = Theorem(
        theorem.name,
        theorem.variableDefinitions,
        newPremises,
        newConclusion,
        newProofs)
    } yield RecalculateReferences(theoremWithContext.copy(entry = newTheorem))._1
  }
  protected def apply(proofWithContext: ProofWithContext): F[Proof] = {
    import proofWithContext._
    for {
      newSteps <- apply(stepsWithContext)
    } yield proof.copy(steps = newSteps)
  }
}
