package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Expression
import net.prover.model.proof.{Premise, Step, SubstitutionContext}

case class Reversal[TComponent <: Expression](relation: BinaryJoiner[TComponent], inference: Inference.Summary) {
  def assertionStep(left: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      relation(left, right),
      inference,
      Seq(Premise.Pending(relation(right, left))),
      relation.fillRequiredSubstitutions(inference.requiredSubstitutions, Seq(right, left)))
  }
}

