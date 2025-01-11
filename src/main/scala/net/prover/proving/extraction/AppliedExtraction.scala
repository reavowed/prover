package net.prover.proving.extraction

import net.prover.model._
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.proving.derivation.DefinitionDeconstructionBase

case class AppliedExtraction(extractionSteps: Seq[AppliedExtractionStep]) extends StepLike.Wrapper {
  override def substeps: Seq[StepLike] = extractionSteps
  def toProofSteps: Seq[Step.AssertionOrExtraction] = extractionSteps.map(_.toProofStep)
}
object AppliedExtraction {
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[AppliedExtraction] = {
    Parser.mapFoldWhileDefined[AppliedExtractionStep, StepContext](stepContext) { (_, currentStepContext) =>
      AppliedExtractionStep.parser(currentStepContext, implicitly)
        .mapMap(step => step -> currentStepContext.addStep(step.toProofStep))
    }.map(_._1).map(AppliedExtraction(_))
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

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[Option[AppliedExtractionStep]] = {
    Parser.selectOptionalWordParser {
      case Step.AssertionStep.label => Step.AssertionStep.parser.map(Assertion)
      case DefinitionDeconstructionBase.label => {
        val innerContext = stepContext.forChild()
        for {
          deconstructionStep <- Parser.requiredWord(Step.AssertionStep.label).flatMap(_ => Step.AssertionStep.parser(innerContext, implicitly))
          stepContextAfterDeconstructionStep = innerContext.addStep(deconstructionStep)
          additionalSteps <- Step.listParser(stepContext =>
            Parser.optionalWord(Step.AssertionStep.label)
              .flatMapMap(_ => Step.AssertionStep.parser(stepContext, implicitly))
          )(stepContextAfterDeconstructionStep).map(_._1)
        } yield DefinitionDeconstruction(deconstructionStep, additionalSteps)
      }.inBraces
    }
  }
}
