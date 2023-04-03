package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.expressions.Statement

sealed trait DerivationStep {
  def statement: Statement
  def step: Step
  def inferences: Seq[Inference]
}
object DerivationStep {
  def fromAssertion(step: Step.Assertion): DerivationStepWithSingleInference = DerivationStepWithSingleInference(step.statement, step.inference, step)

  implicit class SeqOps(derivationSteps: Seq[DerivationStep]) {
    def deduplicate: Seq[DerivationStep] = derivationSteps.distinctBy(_.statement)
    def inferences: Seq[Inference] = derivationSteps.flatMap(_.inferences)
    def steps: Seq[Step] = derivationSteps.map(_.step)
    def elideWithInference(inference: Inference): DerivationStepWithSingleInference = {
      DerivationStepWithSingleInference(
        derivationSteps.last.statement,
        inference,
        Step.Elided.ifNecessary(derivationSteps.steps, inference).get)
    }
  }
}

case class DerivationStepWithSingleInference(statement: Statement, inference: Inference, step: Step) extends DerivationStep {
  override def inferences: Seq[Inference] = Seq(inference)
  def elideWithFollowingSteps(followingSteps: Seq[DerivationStep]): DerivationStepWithSingleInference = {
    if (followingSteps.isEmpty)
      this
    else
      DerivationStepWithSingleInference(followingSteps.last.statement, inference, Step.Elided.forInference(inference)(step +: followingSteps.steps))
  }
}

case class DerivationStepWithMultipleInferences(statement: Statement, inferences: Seq[Inference], step: Step) extends DerivationStep
