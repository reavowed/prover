package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.{Inference, Parser, ParsingContext, Premise}

case class ProofOutline(steps: Seq[StepOutline]) {
  def fillIn(
    premises: Seq[Premise],
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint],
    transformations: Seq[StatementDefinition]
  ): Proof = {
    val context = ProvingContext.getInitial(premises, availableInferences, assertionHints, transformations)
    val detailedSteps = steps.prove(None)(context)
    Proof(detailedSteps)
  }
}

object ProofOutline {
  def parser(implicit context: ParsingContext): Parser[ProofOutline] = {
    StepOutline.listParser.map(ProofOutline.apply)
  }
}
