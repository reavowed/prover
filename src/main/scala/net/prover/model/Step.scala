package net.prover.model

import net.prover.model.Step.Fantasy

case class Step(statement: Statement, deductionId: String, fantasy: Option[Fantasy] = None)

object Step {
  case class Fantasy(assumption: Statement, steps: Seq[Step])
}
