package net.prover.model.definitions

import net.prover.controllers.ExtractionHelper
import net.prover.model.Inference
import net.prover.model.expressions.{Expression, Term}
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.{StepProvingContext, SubstitutionContext}

sealed trait RearrangementOperation {
  def inference: Inference.Summary
  def extractionOption: ExtractionOption
  protected def source(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term
  protected def result(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term
  def rearrangementStep[T <: Expression](terms: Seq[Term], wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    for {
      (assertionStep, targetSteps) <- ExtractionHelper.getInferenceExtractionWithPremises(
        inference,
        extractionOption.extractionInferences,
        extractionOption.requiredSubstitutions.fill(Nil, terms),
        None,
        None
      ).toOption
      if targetSteps.isEmpty
      expansionSteps = expansion.assertionStepIfNecessary(source(terms), result(terms), wrapper).toSeq
    } yield RearrangementStep(wrapper(result(terms)), assertionStep.step +: expansionSteps, inference)
  }
  def reversedRearrangementStep[T <: Expression](terms: Seq[Term], wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    for {
      forwardStep <- rearrangementStep(terms, wrapper, expansion)
      reversalStep = reversal.assertionStep(wrapper(result(terms)), wrapper(source(terms)))
    } yield RearrangementStep(wrapper(source(terms)), forwardStep.substeps :+ reversalStep, inference)
  }
}
object RearrangementOperation {
  trait Unary extends RearrangementOperation {
    def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term
    def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term
    override def source(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = source(terms(0))
    override def result(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = result(terms(0))
    def rearrangementStep[T <: Expression](a: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a), wrapper, expansion)
    def reversedRearrangementStep[T <: Expression](a: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = reversedRearrangementStep(Seq(a), wrapper, expansion, reversal)
  }
  trait Binary extends RearrangementOperation {
    def source(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term
    def result(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term
    override def source(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = source(terms(0), terms(1))
    override def result(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = result(terms(0), terms(1))
    def rearrangementStep[T <: Expression](a: Term, b: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a, b), wrapper, expansion)
    def reversedRearrangementStep[T <: Expression](a: Term, b: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = reversedRearrangementStep(Seq(a, b), wrapper, expansion, reversal)
  }
  trait Ternary extends RearrangementOperation {
    def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term
    def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term
    override def source(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = source(terms(0), terms(1), terms(2))
    override def result(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = result(terms(0), terms(1), terms(2))
    def rearrangementStep[T <: Expression](a: Term, b: Term, c: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a, b, c), wrapper, expansion)
    def reversedRearrangementStep[T <: Expression](a: Term, b: Term, c: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = reversedRearrangementStep(Seq(a, b, c), wrapper, expansion, reversal)
  }
}

case class Associativity(operator: BinaryOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Ternary {
  override def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, operator(b, c))
  override def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(operator(a, b), c)
}
case class Commutativity(operator: BinaryOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Binary {
  override def source(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, b)
  override def result(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(b, a)
}
case class LeftDistributivity(distributor: RearrangeableOperator, distributee: RearrangeableOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Ternary {
  override def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributor(a, distributee(b, c))
  override def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributee(distributor(a, b), distributor(a, c))
}
case class RightDistributivity(distributor: RearrangeableOperator, distributee: RearrangeableOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Ternary {
  override def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributor(distributee(a, b), c)
  override def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributee(distributor(a, c), distributor(b, c))
}
case class LeftIdentity(operator: BinaryOperator, identityTerm: Term, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Unary {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(identityTerm, a)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = a
}
case class RightIdentity(operator: BinaryOperator, identityTerm: Term, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Unary {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, identityTerm)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = a
}
case class LeftAbsorber(operator: BinaryOperator, absorberTerm: Term, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Unary {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(absorberTerm, a)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = absorberTerm
}
case class RightAbsorber(operator: BinaryOperator, absorberTerm: Term, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Unary {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, absorberTerm)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = absorberTerm
}
