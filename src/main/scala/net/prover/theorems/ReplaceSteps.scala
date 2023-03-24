package net.prover.theorems

import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.{Step, StepContext}
import scalaz.Functor
import scalaz.syntax.functor._

object ReplaceSteps {
  def apply[F[_] : Functor](theorem: Theorem, proofIndex: Int, stepIndexes: Seq[Int])(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Theorem]] = {
    theorem.proofs.splitAtIndexIfValid(proofIndex).flatMap { case (before, proof, after) =>
      apply(proof.steps, stepIndexes, theorem.initialStepContext)(f).map(_.map(newSteps => theorem.copy(proofs = (before :+ Proof(newSteps)) ++ after)))
    }
  }
  def apply[F[_] : Functor](steps: Seq[Step], stepIndexes: Seq[Int], outerStepContext: StepContext)(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Seq[Step]]] = {
    stepIndexes match {
      case Nil =>
        f(steps, outerStepContext)
      case head +: tail =>
        steps.splitAtIndexIfValid(head).flatMap { case (before, step, after) =>
          apply(step, tail, outerStepContext.addSteps(before).atIndex(head))(f).map(_.map(newStep => (before :+ newStep) ++ after))
        }
    }
  }
  def apply[F[_] : Functor](step: Step, stepIndexes: Seq[Int], stepContext: StepContext)(f: (Seq[Step], StepContext) => Option[F[Seq[Step]]]): Option[F[Step]] = {
    step match {
      case step: Step.WithSubsteps =>
        apply(step.substeps, stepIndexes, stepContext)(f).map(_.map(step.replaceSubsteps(_, stepContext)))
      case _: Step.WithoutSubsteps =>
        None
    }
  }
}
