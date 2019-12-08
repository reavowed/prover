package net.prover.model.definitions

import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{ProofHelper, Step, StepProvingContext, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}

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

  def forwardRearrangementStep(a: Term, b: Term, c: Term, wrapper: Wrapper[Term, Term])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep] = {
    for {
      (assertionStep, targetSteps) <- ProofHelper.getAssertionWithPremises(
        inference,
        Substitutions(terms = inference.requiredSubstitutions.terms.zip(Seq(a, b, c)).toMap))
      if targetSteps.isEmpty
      expansionSteps = equality.expansion.assertionStepIfNecessary(normalisedTerm(a, b, c), reversedTerm(a, b, c), wrapper).toSeq
    } yield RearrangementStep(wrapper(reversedTerm(a, b, c)), assertionStep +: expansionSteps, inference)
  }

  def reverseRearrangementStep(a: Term, b: Term, c: Term, wrapper: Wrapper[Term, Term])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep] = {
    for {
      forwardStep <- forwardRearrangementStep(a, b, c, wrapper)
      reversalStep = equality.reversal.assertionStep(wrapper(reversedTerm(a, b, c)), wrapper(normalisedTerm(a, b, c)))
    } yield RearrangementStep(wrapper(normalisedTerm(a, b, c)), forwardStep.substeps :+ reversalStep, inference)
  }
}
