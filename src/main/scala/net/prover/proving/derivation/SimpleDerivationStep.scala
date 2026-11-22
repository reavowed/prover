package net.prover.proving.derivation

import net.prover.model.*
import net.prover.model.proof.{Step, StepContext}
import net.prover.parsing.KnownWordParser
import net.prover.proving.extraction.AppliedInferenceExtraction

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
