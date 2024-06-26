package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Expression
import net.prover.model.proof.{Premise, Step, SubstitutionContext}

case class Reversal[TComponent <: Expression](joiner: BinaryJoiner[TComponent], inference: Inference.Summary) {
  def assertionStep(left: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    Step.AssertionStep(
      joiner(left, right),
      inference,
      Seq(Premise.Pending(joiner(right, left))),
      joiner.fillSubstitutions(Seq(right, left)))
  }
}

