package net.prover.proving.derivation

import net.prover.model.ProvingContext
import net.prover.model.proof.{Step, StepContext}
import net.prover.parsing.KnownWordParser

case class DefinitionDeconstruction(
  deconstructionStep: Step.AssertionStep,
  additionalSteps: Seq[Step.AssertionStep]
) extends DefinitionDeconstructionBase

object DefinitionDeconstruction {
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): KnownWordParser[DefinitionDeconstruction] = KnownWordParser(DefinitionDeconstructionBase.label) {
    (for {
      deconstructionStep <- Step.AssertionStep.parser(stepContext.forChild(), implicitly)
      additionalSteps <- Step.listParser(stepContext =>
        Step.AssertionStep.parser(stepContext, implicitly)
      )(stepContext.forChild().addStep(deconstructionStep)).map(_._1)
    } yield DefinitionDeconstruction(deconstructionStep, additionalSteps)).inBraces
  }
}
