package net.prover.proving.structure.inferences

import net.prover.model.Inference
import net.prover.model.proof.Step
import net.prover.proving.derivation.DerivedStatement

/**
  * An inference such as `φ, ψ ⊢ φ ∧ ψ` or `a ∈ ℕ, b ∈ ℕ ⊢ a + b ∈ ℕ` that can be run in reverse, to allow a target
  * conclusion to be decomposed into smaller parts.
  */
sealed trait ConclusionSimplificationInference {
  def inference: Inference
  /**
    * The derivation of this inference's conclusion from derivations of its premises.
    */
  def derive(premiseDerivations: Seq[DerivedStatement], step: Step.AssertionStep): DerivedStatement
}

object ConclusionSimplificationInference {
  /**
    * An inference that only builds up logical structure, e.g. `φ, ψ ⊢ φ ∧ ψ` or `φ ⊢ φ ∨ ψ`.
    */
  case class Combination(inference: Inference) extends ConclusionSimplificationInference {
    override def derive(premiseDerivations: Seq[DerivedStatement], step: Step.AssertionStep): DerivedStatement = {
      DerivedStatement.Combined(premiseDerivations, step)
    }
  }
  /**
    * An inference that builds up a statement about terms, e.g. `a ∈ ℕ, b ∈ ℕ ⊢ a + b ∈ ℕ`.
    */
  case class Composition(inference: Inference) extends ConclusionSimplificationInference {
    override def derive(premiseDerivations: Seq[DerivedStatement], step: Step.AssertionStep): DerivedStatement = {
      DerivedStatement.Composed(premiseDerivations, step)
    }
  }

  def apply(inference: Inference): ConclusionSimplificationInference = {
    if (inference.variableDefinitions.terms.isEmpty) Combination(inference) else Composition(inference)
  }
}
