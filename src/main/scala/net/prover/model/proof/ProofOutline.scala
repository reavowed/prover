package net.prover.model.proof

import net.prover.model.{Parser, ParsingContext}

case class ProofOutline(steps: Seq[StepOutline])

object ProofOutline {
  def parser(implicit context: ParsingContext): Parser[ProofOutline] = {
    StepOutline.listParser.map(ProofOutline.apply)
  }
}
