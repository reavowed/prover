package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}

case class Expansion(relation: BinaryJoiner[Term], inference: Inference.Summary) {
  def assertionStep(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      relation(wrapper(left), wrapper(right)),
      inference,
      Seq(Premise.Pending(relation(left, right))),
      Substitutions(
        terms = inference.requiredSubstitutions.terms.zip(Seq(left, right)).toMap,
        functions = inference.requiredSubstitutions.functions.zip(Seq(wrapper.template)).toMap))
  }

  def assertionStepIfNecessary(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): Option[Step.Assertion] = {
    if (wrapper.isIdentity) {
      None
    } else {
      Some(assertionStep(left, right, wrapper))
    }
  }
}
