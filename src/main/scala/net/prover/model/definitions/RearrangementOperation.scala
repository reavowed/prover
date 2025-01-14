package net.prover.model.definitions

import net.prover.model.expressions.{Expression, Term}
import net.prover.model.proof.{StepProvingContext, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.proving.extraction.{ExtractionApplier, InferenceExtraction}
import net.prover.proving.structure.inferences.{Expansion, Reversal}
import net.prover.util.Direction

sealed trait RearrangementOperation {
  def inferenceExtraction: InferenceExtraction
  def inference: Inference = inferenceExtraction.inference
  def source(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term
  def result(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term
  def rearrangementStep[T <: Expression](terms: Seq[Term], wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    for {
      (assertionStep, targetSteps) <- ExtractionApplier.getInferenceExtractionStepWithPremises(
        inferenceExtraction,
        Substitutions(Nil, terms),
        Nil,
        None,
        None
      ).toOption
      if targetSteps.isEmpty
      expansionSteps = expansion.assertionStepIfNecessary(source(terms), result(terms), wrapper).toSeq
    } yield RearrangementStep(wrapper(result(terms)), assertionStep +: expansionSteps, inference.summary)
  }
  def reversedRearrangementStep[T <: Expression](terms: Seq[Term], wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    for {
      forwardStep <- rearrangementStep(terms, wrapper, expansion)
      reversalStep = reversal.assertionStep(wrapper(result(terms)), wrapper(source(terms)))
    } yield RearrangementStep(wrapper(source(terms)), forwardStep.substeps :+ reversalStep, inference.summary)
  }
  def rearrangementStep[T <: Expression](terms: Seq[Term], direction: Direction, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = {
    direction.getSource(
      rearrangementStep(terms, wrapper, expansion),
      reversedRearrangementStep(terms, wrapper, expansion, reversal))
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
    def rearrangementStep[T <: Expression](a: Term, direction: Direction, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a), direction, wrapper, expansion, reversal)
  }
  trait Binary extends RearrangementOperation {
    def source(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term
    def result(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term
    override def source(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = source(terms(0), terms(1))
    override def result(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = result(terms(0), terms(1))
    def rearrangementStep[T <: Expression](a: Term, b: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a, b), wrapper, expansion)
    def reversedRearrangementStep[T <: Expression](a: Term, b: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = reversedRearrangementStep(Seq(a, b), wrapper, expansion, reversal)
    def rearrangementStep[T <: Expression](a: Term, b: Term, direction: Direction, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a, b), direction, wrapper, expansion, reversal)
  }
  trait Ternary extends RearrangementOperation {
    def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term
    def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term
    override def source(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = source(terms(0), terms(1), terms(2))
    override def result(terms: Seq[Term])(implicit substitutionContext: SubstitutionContext): Term = result(terms(0), terms(1), terms(2))
    def rearrangementStep[T <: Expression](a: Term, b: Term, c: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a, b, c), wrapper, expansion)
    def reversedRearrangementStep[T <: Expression](a: Term, b: Term, c: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = reversedRearrangementStep(Seq(a, b, c), wrapper, expansion, reversal)
    def rearrangementStep[T <: Expression](a: Term, b: Term, c: Term, direction: Direction, wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(Seq(a, b, c), direction, wrapper, expansion, reversal)
  }
}

case class Associativity(operator: BinaryOperator, inferenceExtraction: InferenceExtraction) extends RearrangementOperation.Ternary {
  override def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, operator(b, c))
  override def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(operator(a, b), c)
}
case class Commutativity(operator: BinaryOperator, inferenceExtraction: InferenceExtraction) extends RearrangementOperation.Binary {
  override def source(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, b)
  override def result(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(b, a)
  override def reversedRearrangementStep[T <: Expression](terms: Seq[Term], wrapper: Wrapper[Term, T], expansion: Expansion[T], reversal: Reversal[T])(implicit stepProvingContext: StepProvingContext): Option[RearrangementStep[T]] = rearrangementStep(terms.reverse, wrapper, expansion)
}
trait Distributivity extends RearrangementOperation.Ternary {
  def distributor: RearrangeableOperator
  def distributee: RearrangeableOperator
}
case class LeftDistributivity(distributor: RearrangeableOperator, distributee: RearrangeableOperator, inferenceExtraction: InferenceExtraction) extends Distributivity {
  override def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributor(a, distributee(b, c))
  override def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributee(distributor(a, b), distributor(a, c))
}
case class RightDistributivity(distributor: RearrangeableOperator, distributee: RearrangeableOperator, inferenceExtraction: InferenceExtraction) extends Distributivity {
  override def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributor(distributee(a, b), c)
  override def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributee(distributor(a, c), distributor(b, c))
}
trait OneSidedIdentity extends RearrangementOperation.Unary {
  def operator: BinaryOperator
  def identityTerm: Term
}
case class LeftIdentity(operator: BinaryOperator, identityTerm: Term, inferenceExtraction: InferenceExtraction) extends OneSidedIdentity {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(identityTerm, a)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = a
}
case class RightIdentity(operator: BinaryOperator, identityTerm: Term, inferenceExtraction: InferenceExtraction) extends OneSidedIdentity {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, identityTerm)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = a
}
case class DoubleSidedIdentity(operator: BinaryOperator, identityTerm: Term, leftIdentity: LeftIdentity, rightIdentity: RightIdentity)

trait OneSidedAbsorber extends RearrangementOperation.Unary {
  def operator: BinaryOperator
  def absorberTerm: Term
}
case class LeftAbsorber(operator: BinaryOperator, absorberTerm: Term, inferenceExtraction: InferenceExtraction) extends OneSidedAbsorber {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(absorberTerm, a)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = absorberTerm
}
case class RightAbsorber(operator: BinaryOperator, absorberTerm: Term, inferenceExtraction: InferenceExtraction) extends OneSidedAbsorber {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, absorberTerm)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = absorberTerm
}

case class RightInverse(binaryOperator: BinaryOperator, inverseOperator: UnaryOperator, identity: DoubleSidedIdentity, inferenceExtraction: InferenceExtraction) extends RearrangementOperation.Unary {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = binaryOperator(a, inverseOperator(a))
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = identity.identityTerm
}
case class LeftInverse(binaryOperator: BinaryOperator, inverseOperator: UnaryOperator, identity: DoubleSidedIdentity, inferenceExtraction: InferenceExtraction) extends RearrangementOperation.Unary {
  override def source(a: Term)(implicit substitutionContext: SubstitutionContext): Term = binaryOperator(inverseOperator(a), a)
  override def result(a: Term)(implicit substitutionContext: SubstitutionContext): Term = identity.identityTerm
}
case class DoubleSidedInverse(binaryOperator: BinaryOperator, inverseOperator: UnaryOperator, identity: DoubleSidedIdentity, rightInverse: RightInverse, leftInverse: LeftInverse)

case class LeftOperatorExtraction(unaryOperator: UnaryOperator, binaryOperator: BinaryOperator, inferenceExtraction: InferenceExtraction) extends RearrangementOperation.Binary {
  override def source(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = binaryOperator(unaryOperator(a), b)
  override def result(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = unaryOperator(binaryOperator(a, b))
}
case class RightOperatorExtraction(unaryOperator: UnaryOperator, binaryOperator: BinaryOperator, inferenceExtraction: InferenceExtraction) extends RearrangementOperation.Binary {
  override def source(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = binaryOperator(a, unaryOperator(b))
  override def result(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = unaryOperator(binaryOperator(a, b))
}
