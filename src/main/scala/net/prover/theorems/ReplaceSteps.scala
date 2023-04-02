package net.prover.theorems

import net.prover.entries.{StepWithContext, StepsWithContext, TheoremWithContext}
import net.prover.model.SeqOps
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.Step
import scalaz.Functor
import scalaz.syntax.functor._

object ReplaceSteps {
  def apply[F[_] : Functor](theoremWithContext: TheoremWithContext, proofIndex: Int, stepIndexes: Seq[Int])(f: StepsWithContext => Option[F[Seq[Step]]]): Option[F[Theorem]] = {
    theoremWithContext.theorem.proofs.splitAtIndexIfValid(proofIndex).flatMap { case (before, proof, after) =>
      apply(theoremWithContext.atProof(proof).stepsWithContext, stepIndexes)(f)
        .map(_.map(newSteps => theoremWithContext.theorem.copy(proofs = (before :+ Proof(newSteps)) ++ after)))
    }
  }
  def apply[F[_] : Functor](stepsWithContext: StepsWithContext, stepIndexes: Seq[Int])(f: StepsWithContext => Option[F[Seq[Step]]]): Option[F[Seq[Step]]] = {
    stepIndexes match {
      case Nil =>
        f(stepsWithContext)
      case head +: tail =>
        stepsWithContext.steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
          apply(stepsWithContext.atChild(before, step), tail)(f).map(_.map(newStep => (before :+ newStep) ++ after))
        }
    }
  }
  def apply[F[_] : Functor](stepWithContext: StepWithContext, stepIndexes: Seq[Int])(f: StepsWithContext => Option[F[Seq[Step]]]): Option[F[Step]] = {
    stepWithContext.step match {
      case step: Step.WithSubsteps =>
        apply(stepWithContext.forSubsteps(step), stepIndexes)(f).map(_.map(step.replaceSubsteps(_)(stepWithContext.stepProvingContext)))
      case _: Step.WithoutSubsteps =>
        None
    }
  }
}
