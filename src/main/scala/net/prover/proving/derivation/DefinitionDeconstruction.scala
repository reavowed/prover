package net.prover.proving.derivation

import net.prover.model.proof.Step

case class DefinitionDeconstruction(
  deconstructionStep: Step.AssertionStep,
  additionalSteps: Seq[Step.AssertionStep]
) extends DefinitionDeconstructionBase
