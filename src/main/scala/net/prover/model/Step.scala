package net.prover.model

import net.prover.model.Step.Fantasy

case class Step(statement: Statement, inferenceId: String, fantasy: Option[Fantasy] = None)

object Step {
  case class Fantasy(assumption: Statement, steps: Seq[Step])
}
