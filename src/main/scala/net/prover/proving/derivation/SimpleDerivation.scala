package net.prover.proving.derivation

import net.prover.model._
import net.prover.model.proof.{Step, StepLike}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction}

case class SimpleDerivation(steps: Seq[SimpleDerivationStep]) extends StepLike.Wrapper {
  def :+(inferenceExtraction: AppliedInferenceExtraction): SimpleDerivation = {
    this :+ SimpleDerivationStep.InferenceExtraction(inferenceExtraction)
  }
  def :+(assertionStep: Step.AssertionStep): SimpleDerivation = {
    this :+ SimpleDerivationStep.Assertion(assertionStep)
  }
  def :+(step: SimpleDerivationStep): SimpleDerivation = {
    this ++ Seq(step)
  }
  def +:(assertionStep: Step.AssertionStep): SimpleDerivation = {
    SimpleDerivationStep.Assertion(assertionStep) +: this
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
  override def substeps: Seq[StepLike] = steps
  def inferences: Set[Inference] = steps.map(_.inference).toSet
  def toProofSteps: Seq[Step.AssertionOrExtraction] = steps.map(_.toProofStep)
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
  def fromAssertions(assertions: Seq[Step.AssertionStep]): SimpleDerivation = {
    SimpleDerivation(assertions.map(SimpleDerivationStep.Assertion))
  }
  def fromExtraction(appliedExtraction: AppliedExtraction): SimpleDerivation = {
    SimpleDerivation(appliedExtraction.extractionSteps.map {
      case AppliedExtractionStep.Assertion(assertionStep) => SimpleDerivationStep.Assertion(assertionStep)
      case AppliedExtractionStep.DefinitionDeconstruction(deconstructionStep, additionalSteps) => SimpleDerivationStep.DefinitionDeconstruction(deconstructionStep, additionalSteps)
    })
  }
}

sealed trait SimpleDerivationStep extends StepLike.Wrapper {
  def inference: Inference
  def toProofStep: Step.AssertionOrExtraction
}
object SimpleDerivationStep {
  case class Assertion(assertionStep: Step.AssertionStep) extends SimpleDerivationStep {
    override def inference: Inference = assertionStep.inference
    override def substeps: Seq[StepLike] = Seq(assertionStep)
    override def toProofStep: Step.AssertionOrExtraction = assertionStep
  }
  case class DefinitionDeconstruction(
    deconstructionStep: Step.AssertionStep,
    additionalSteps: Seq[Step.AssertionStep]
  ) extends SimpleDerivationStep with DefinitionDeconstructionBase
  case class InferenceExtraction(appliedInferenceExtraction: AppliedInferenceExtraction) extends SimpleDerivationStep {
    override def inference: Inference = appliedInferenceExtraction.assertionStep.inference
    override def substeps: Seq[StepLike] = Seq(appliedInferenceExtraction)
    override def toProofStep: Step.AssertionOrExtraction = {
      Step.InferenceExtractionStep.ifNecessary(appliedInferenceExtraction)
    }
  }
  implicit def fromSeq[T](seq: Seq[T])(implicit f: T => SimpleDerivationStep): Seq[SimpleDerivationStep] = seq.map(f)
}
