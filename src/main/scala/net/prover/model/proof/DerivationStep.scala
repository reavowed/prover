package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.expressions.Statement

case class DerivationStep(statement: Statement, inference: Inference, step: Step) {
  def elideWithPremiseSteps(premiseSteps: Seq[DerivationStep]): DerivationStep = {
    if (premiseSteps.isEmpty)
      this
    else
      DerivationStep(statement, inference, Step.Elided.forInference(inference)(premiseSteps.steps :+ step))
  }
  def elideWithFollowingSteps(followingSteps: Seq[DerivationStep]): DerivationStep = {
    if (followingSteps.isEmpty)
      this
    else
      DerivationStep(followingSteps.last.statement, inference, Step.Elided.forInference(inference)(step +: followingSteps.steps))
  }
}

object DerivationStep {
  def fromAssertion(step: Step.Assertion): DerivationStep = DerivationStep(step.statement, step.inference, step)

  implicit class SeqOps(premiseSteps: Seq[DerivationStep]) {
    def deduplicate: Seq[DerivationStep] = premiseSteps.distinctBy(_.statement)
    def steps: Seq[Step] = premiseSteps.map(_.step)
    def inferences: Seq[Inference] = premiseSteps.map(_.inference)
  }
}
