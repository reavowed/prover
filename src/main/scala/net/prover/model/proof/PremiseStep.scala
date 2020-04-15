package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.expressions.Statement

case class PremiseStep(statement: Statement, inference: Inference, step: Step)

object PremiseStep {
  def fromAssertion(step: Step.Assertion): PremiseStep = PremiseStep(step.statement, step.inference, step)

  implicit class SeqOps(premiseSteps: Seq[PremiseStep]) {
    def deduplicate: Seq[PremiseStep] = premiseSteps.distinctBy(_.statement)
    def steps: Seq[Step] = premiseSteps.map(_.step)
    def inferences: Seq[Inference] = premiseSteps.map(_.inference)
  }
}
