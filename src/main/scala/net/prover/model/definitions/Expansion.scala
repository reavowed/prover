package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Term
import net.prover.model.proof.{Premise, Step, SubstitutionContext}

case class Expansion(relation: BinaryJoiner[Term], inference: Inference.Summary) {
  def assertionStep(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      relation(wrapper(left), wrapper(right)),
      inference,
      Seq(Premise.Pending(relation(left, right))),
      inference.requiredSubstitutions.fill(Nil, Seq(left, right, wrapper.template)))
  }

  def assertionStepIfNecessary(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): Option[Step.Assertion] = {
    if (wrapper.isIdentity) {
      None
    } else {
      Some(assertionStep(left, right, wrapper))
    }
  }
}
