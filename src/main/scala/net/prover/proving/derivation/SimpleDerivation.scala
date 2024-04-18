package net.prover.proving.derivation

import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.proving.extraction.AppliedInferenceExtraction

case class SimpleDerivation(steps: Seq[SimpleDerivationStep]) {
  def :+(step: SimpleDerivationStep): SimpleDerivation = {
    this ++ Seq(step)
  }
  def +:(step: SimpleDerivationStep): SimpleDerivation = {
    SimpleDerivation(step +: steps)
  }
  def ++(other: SimpleDerivation): SimpleDerivation = {
    this ++ other.steps
  }
  def ++(otherSteps: Seq[SimpleDerivationStep]): SimpleDerivation = {
    SimpleDerivation(steps ++ otherSteps)
  }
  def toProofSteps: Seq[Step.AssertionOrExtraction] = steps.map(_.toProofStep)
  def statement: Statement = steps.lastOption.map(_.statement).getOrElse(throw new IllegalStateException("Derivation is empty"))
  def inferences: Set[Inference] = steps.map(_.inference).toSet
  def nonEmpty: Boolean = steps.nonEmpty
  def distinct: SimpleDerivation = SimpleDerivation(steps.distinctBy(_.statement))
}
object SimpleDerivation {
  val empty: SimpleDerivation = SimpleDerivation(Nil)
  implicit class SeqOps(derivations: Seq[SimpleDerivation]) {
    def join: SimpleDerivation = {
      SimpleDerivation(derivations.flatMap(_.steps))
    }
  }
}

sealed trait SimpleDerivationStep {
  def inference: Inference
  def statement: Statement
  def toProofStep: Step.AssertionOrExtraction
}
object SimpleDerivationStep {
  case class Assertion(assertionStep: Step.AssertionStep) extends SimpleDerivationStep {
    override def inference: Inference = assertionStep.inference
    override def statement: Statement = assertionStep.statement
    override def toProofStep: Step.AssertionStep = assertionStep
  }
  case class Simplification(appliedSimplification: AppliedInferenceExtraction) extends SimpleDerivationStep {
    override def inference: Inference = appliedSimplification.assertionStep.inference
    override def statement: Statement = appliedSimplification.statement
    override def toProofStep: Step.AssertionOrExtraction = appliedSimplification.toStep
  }

  implicit def fromAssertionStep(step: Step.AssertionStep): Assertion = Assertion(step)
  implicit def fromSimplification(appliedSimplification: AppliedInferenceExtraction): Simplification = Simplification(appliedSimplification)
  implicit def fromSeq[T](seq: Seq[T])(implicit f: T => SimpleDerivationStep): Seq[SimpleDerivationStep] = seq.map(f)
}
