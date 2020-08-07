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

  implicit class SeqOps(premiseSteps: Seq[DerivationStep]) {
    def deduplicate: Seq[DerivationStep] = premiseSteps.distinctBy(_.statement)
    def inferences: Seq[Inference] = premiseSteps.flatMap(_.inferences)
    def steps: Seq[Step] = premiseSteps.map(_.step)
    def elideWithInference(inference: Inference): DerivationStepWithSingleInference = {
      DerivationStepWithSingleInference(premiseSteps.last.statement, inference, Step.Elided.ifNecessary(premiseSteps.steps, inference).get)
    }
  }
}

case class DerivationStepWithSingleInference(statement: Statement, inference: Inference, step: Step) extends DerivationStep {
  override def inferences: Seq[Inference] = Seq(inference)
  def elideWithPremiseSteps(premiseSteps: Seq[DerivationStep]): DerivationStepWithSingleInference = {
    if (premiseSteps.isEmpty)
      this
    else
      DerivationStepWithSingleInference(statement, inference, Step.Elided.forInference(inference)(premiseSteps.steps :+ step))
  }
  def elideWithFollowingSteps(followingSteps: Seq[DerivationStep]): DerivationStepWithSingleInference = {
    if (followingSteps.isEmpty)
      this
    else
      DerivationStepWithSingleInference(followingSteps.last.statement, inference, Step.Elided.forInference(inference)(step +: followingSteps.steps))
  }
}

case class DerivationStepWithMultipleInferences(statement: Statement, inferences: Seq[Inference], step: Step) extends DerivationStep
