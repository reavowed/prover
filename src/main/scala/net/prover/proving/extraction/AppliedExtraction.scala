package net.prover.proving.extraction

import net.prover.model._
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.parsing.{KnownWordParser, Parser}
import net.prover.proving.derivation.DefinitionDeconstructionBase

case class AppliedExtraction(extractionSteps: Seq[AppliedExtractionStep], chainedRewriteSteps: Seq[Step.AssertionStep]) extends StepLike.Wrapper {
  override def substeps: Seq[StepLike] = extractionSteps ++ chainedRewriteSteps
  def nonEmpty: Boolean = substeps.nonEmpty
  def toProofSteps: Seq[Step.AssertionOrExtraction] = extractionSteps.map(_.toProofStep) ++ chainedRewriteSteps
  def inferences: Seq[Inference] = {
    (extractionSteps flatMap {
      case AppliedExtractionStep.Assertion(step) => Seq(step.inference)
      case AppliedExtractionStep.DefinitionDeconstruction(step, steps) => (step +: steps).map(_.inference)
    }) ++ chainedRewriteSteps.map(_.inference)
  }

  override def serializedLines: Seq[String] = {
    extractionSteps.flatMap(_.serializedLines) ++
      Seq(chainedRewriteSteps)
        .filter(_.nonEmpty)
        .flatMap(_.flatMap(_.serializedLines).indentInLabelledBracesIfPresent("chainedRewrite"))
  }
}
object AppliedExtraction {
  def fromSimpleExtraction(extractionSteps: Seq[Step.AssertionStep]): AppliedExtraction = {
    AppliedExtraction(extractionSteps.map(AppliedExtractionStep.Assertion(_)), Nil)
  }
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[AppliedExtraction] = {
    for {
      extractionStepsAndContext <- KnownWordParser.mapFoldWhileDefined(stepContext) { stepContext =>
        AppliedExtractionStep.parser(stepContext, implicitly)
          .map(step => step -> stepContext.addStep(step.toProofStep))
      }
      (extractionSteps, stepContext) = extractionStepsAndContext
      rewriteSteps <- KnownWordParser("chainedRewrite") {
        Parser.mapFoldWhileDefined[Step.AssertionStep, StepContext](stepContext) { (_, stepContext) =>
          Step.AssertionStep.parser(stepContext, implicitly).optional.mapMap(step => step -> stepContext.addStep(step))
        }.inBraces.map(_._1)
      }.optional.map(_.getOrElse(Nil))
    } yield AppliedExtraction(extractionSteps, rewriteSteps)
  }
}

sealed trait AppliedExtractionStep extends StepLike.Wrapper {
  def toProofStep: Step.AssertionOrExtraction
}
object AppliedExtractionStep {
  case class Assertion(assertionStep: Step.AssertionStep) extends AppliedExtractionStep {
    override def substeps: Seq[StepLike] = Seq(assertionStep)
    override def toProofStep: Step.AssertionOrExtraction = assertionStep
  }
  case class DefinitionDeconstruction(
    deconstructionStep: Step.AssertionStep,
    additionalSteps: Seq[Step.AssertionStep]
  ) extends AppliedExtractionStep
    with DefinitionDeconstructionBase

  def apply(step: Step.AssertionOrExtraction): AppliedExtractionStep = step match {
    case step: Step.AssertionStep => Assertion(step)
    case step: Step.InferenceExtractionStep => DefinitionDeconstruction(
      step.inferenceExtraction.assertionStep,
      step.inferenceExtraction.extraction.extractionSteps.collect { case AppliedExtractionStep.Assertion(step) => step })
  }

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): KnownWordParser[AppliedExtractionStep] = {
    val assertionParser = Step.AssertionStep.parser.map(Assertion(_))
    val definitionParser = KnownWordParser("deconstruction") {
      val innerContext = stepContext.forChild()
      (for {
        deconstructionStep <- Step.AssertionStep.parser(innerContext, implicitly)
        stepContextAfterDeconstructionStep = innerContext.addStep(deconstructionStep)
        additionalSteps <- Step.listParser(stepContext =>
          Step.AssertionStep.parser(stepContext, implicitly)
        )(stepContextAfterDeconstructionStep).map(_._1)
      } yield DefinitionDeconstruction(deconstructionStep, additionalSteps)).inBraces
    }
    KnownWordParser.select(Seq(assertionParser, definitionParser))
  }
}
