package net.prover.entries

import net.prover.model.proof.{Step, StepContext}
import net.prover.model.{AvailableEntries, ProvingContext}

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class StepsWithContext(
    steps: Seq[Step],
    outerStepContext: StepContext,
    proofWithContext: ProofWithContext)
{
  def globalContext: GlobalContext = proofWithContext.globalContext
  def availableEntries: AvailableEntries = proofWithContext.availableEntries
  def provingContext: ProvingContext = proofWithContext.provingContext
  def stepsWithContexts: Seq[StepWithContext] = {
    @tailrec def helper(before: Seq[Step], next: Seq[Step], soFar: Seq[StepWithContext]): Seq[StepWithContext] = {
      next match {
        case head +: tail =>
          helper(before :+ head, tail, soFar :+ atChild(before, head))
        case Nil =>
          soFar
      }
    }
    helper(Nil, steps, Nil)
  }
  def atIndex(index: Int): Option[StepWithContext] = {
    steps.splitAtIndexIfValid(index).map { case (before, step, _) => atChild(before, step) }
  }
  def atChild[T <: Step : ClassTag](before: Seq[Step], step: T): TypedStepWithContext[T] = {
    val index = before.length
    TypedStepWithContext(
      step,
      proofWithContext)(
      implicitly,
      outerStepContext.addSteps(before).atIndex(index))
  }
}
