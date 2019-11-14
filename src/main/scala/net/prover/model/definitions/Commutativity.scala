package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model.{Inference, Substitutions}
import net.prover.model.proof.{ProofHelper, StepContext}

case class Commutativity(operator: BinaryOperator, inference: Inference.Summary, equality: Equality) {
  def rearrangementStep(a: Term, b: Term, wrapper: Wrapper[Term, Term])(implicit stepContext: StepContext): Option[RearrangementStep] = {
    for {
      commutativitySteps <- ProofHelper.getAssertionWithPremises(
        inference,
        Substitutions(terms = inference.requiredSubstitutions.terms.zip(Seq(a, b)).toMap))
      expansionSteps = equality.expansion.assertionStepIfNecessary(operator(a, b), operator(b, a), wrapper).toSeq
    } yield RearrangementStep(wrapper(operator(b, a)), commutativitySteps ++ expansionSteps, inference)
  }
}
