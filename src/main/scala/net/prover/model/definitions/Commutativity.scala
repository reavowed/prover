package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.{Expression, Term}
import net.prover.model.proof.{ProofHelper, StepProvingContext}

case class Commutativity(operator: BinaryOperator, inference: Inference.Summary, equality: Equality) {
  def rearrangementStep[T <: Expression](a: Term, b: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    for {
      (assertionStep, targetSteps) <- ProofHelper.getAssertionWithPremisesAndElide(
        inference,
        inference.requiredSubstitutions.fill(Nil, Seq(a, b)))
      if targetSteps.isEmpty
      expansionSteps = expansion.assertionStepIfNecessary(operator(a, b), operator(b, a), wrapper).toSeq
    } yield RearrangementStep(wrapper(operator(b, a)), assertionStep +: expansionSteps, inference)
  }
}
