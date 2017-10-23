package net.prover.model.proof

import net.prover.model.{Parser, ParsingContext}

case class ProofOutline(steps: Seq[StepOutline]) {
  def fillIn(implicit provingContext: ProvingContext): Proof = {
    val detailedSteps = steps.prove(None)
    Proof(detailedSteps)
  }
}

object ProofOutline {
  def parser(implicit context: ParsingContext): Parser[ProofOutline] = {
    StepOutline.listParser.map(ProofOutline.apply)
  }
}
