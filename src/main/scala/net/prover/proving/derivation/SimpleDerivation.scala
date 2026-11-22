package net.prover.proving.derivation

import net.prover.model.*
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.parsing.{KnownWordParser, Parser}
import net.prover.proving.derivation.SimpleDerivationStep.{inference, toProofStep}
import net.prover.proving.extraction.AppliedInferenceExtraction

case class SimpleDerivation(steps: Seq[SimpleDerivationStep]) extends StepLike.Wrapper {
  override def substeps: Seq[StepLike] = steps
  def inferences: Set[Inference] = steps.map(_.inference).toSet
  def nonEmpty: Boolean = steps.nonEmpty
  def isEmpty: Boolean = steps.isEmpty
}
object SimpleDerivation {
  val empty: SimpleDerivation = SimpleDerivation(Nil)
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[SimpleDerivation] = {
    StepLike.listParser[SimpleDerivationStep](
      SimpleDerivationStep.parser(_, implicitly),
      (sc, sds) => sc.addStep(sds.toProofStep)
    ).map(_._1).map(SimpleDerivation(_))
  }
}

type SimpleDerivationStep = Step.AssertionStep | DefinitionDeconstruction | AppliedInferenceExtraction

object SimpleDerivationStep {
  extension (step: SimpleDerivationStep) {
    def toProofStep: Step.AssertionOrExtraction = step match {
      case assertionStep: Step.AssertionStep => assertionStep
      case definitionDeconstruction: DefinitionDeconstruction => definitionDeconstruction.toProofStep
      case appliedInferenceExtraction: AppliedInferenceExtraction => appliedInferenceExtraction.toProofStep
    }
    def inference: Inference = toProofStep.inference
  }
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): KnownWordParser[SimpleDerivationStep] = {
    val assertionParser = Step.AssertionStep.parser
    val definitionParser = DefinitionDeconstruction.parser
    val extractionParser = AppliedInferenceExtraction.parser
    KnownWordParser.select(Seq(assertionParser, definitionParser, extractionParser))
  }

  implicit def fromSeq[T](seq: Seq[T])(implicit f: T => SimpleDerivationStep): Seq[SimpleDerivationStep] = seq.map(f)
}
