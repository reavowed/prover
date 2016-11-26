package net.prover.model

import net.prover.model.Step.Fantasy

case class Step(statement: Statement, fantasy: Option[Fantasy] = None)

object Step {
  case class Fantasy(hypothesis: Statement, steps: Seq[Step])
}
