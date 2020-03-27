package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.{ProofHelper, StepProvingContext, SubstitutionContext}

case class Associativity(operator: BinaryOperator, inference: Inference.Summary, equality: Equality) {

  def normalisedTerm(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = {
    operator(a, operator(b, c))
  }
  def reversedTerm(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = {
    operator(operator(a, b), c)
  }

  def forwardStatement(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Statement = {
    equality(normalisedTerm(a, b, c), reversedTerm(a, b ,c))
  }
  def reverseStatement(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Statement = {
    equality(reversedTerm(a, b, c), normalisedTerm(a, b, c))
  }

  def forwardRearrangementStep[T <: Expression](a: Term, b: Term, c: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    for {
      (assertionStep, targetSteps) <- ProofHelper.getAssertionWithPremisesAndElide(
        inference,
        inference.requiredSubstitutions.fill(Nil, Seq(a, b, c)))
      if targetSteps.isEmpty
      expansionSteps = expansion.assertionStepIfNecessary(normalisedTerm(a, b, c), reversedTerm(a, b, c), wrapper).toSeq
    } yield RearrangementStep(wrapper(reversedTerm(a, b, c)), assertionStep +: expansionSteps, inference)
  }

  def reverseRearrangementStep[T <: Expression](a: Term, b: Term, c: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    for {
      forwardStep <- forwardRearrangementStep(a, b, c, wrapper, expansion)
      reversalStep = reversal.assertionStep(wrapper(reversedTerm(a, b, c)), wrapper(normalisedTerm(a, b, c)))
    } yield RearrangementStep(wrapper(normalisedTerm(a, b, c)), forwardStep.substeps :+ reversalStep, inference)
  }
}
