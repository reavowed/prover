package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.expressions.Statement

case class DerivationStep(statement: Statement, inference: Inference, step: Step)

object DerivationStep {
  def fromAssertion(step: Step.Assertion): DerivationStep = DerivationStep(step.statement, step.inference, step)

  implicit class SeqOps(premiseSteps: Seq[DerivationStep]) {
    def deduplicate: Seq[DerivationStep] = premiseSteps.distinctBy(_.statement)
    def steps: Seq[Step] = premiseSteps.map(_.step)
    def inferences: Seq[Inference] = premiseSteps.map(_.inference)
  }
}
