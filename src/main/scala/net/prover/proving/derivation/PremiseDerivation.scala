package net.prover.proving.derivation

import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.model.unwrapping.Unwrapper
import net.prover.proving.extraction.AppliedInferenceExtraction

class PremiseDerivation private(val steps: Seq[PremiseDerivationStep]) {
  def :+(step: PremiseDerivationStep): PremiseDerivation = {
    this ++ Seq(step)
  }
  def ++(other: PremiseDerivation): PremiseDerivation = {
    this ++ other.steps
  }
  def ++(otherSteps: Seq[PremiseDerivationStep]): PremiseDerivation = {
    PremiseDerivation(steps ++ otherSteps)
  }
  def statement: Statement = steps.lastOption.map(_.statement).getOrElse(throw new IllegalStateException("Derivation is empty"))
  def proofSteps: Seq[Step] = steps.flatMap(_.proofSteps)
}

object PremiseDerivation {
  val empty: PremiseDerivation = PremiseDerivation(Nil)
  def apply(steps: Seq[PremiseDerivationStep]): PremiseDerivation = new PremiseDerivation(steps.distinctBy(_.statement))
  implicit class SeqOps(derivations: Seq[PremiseDerivation]) {
    def join: PremiseDerivation = {
      PremiseDerivation(derivations.flatMap(_.steps))
    }
  }
}

sealed trait PremiseDerivationStep {
  def statement: Statement
  def proofSteps: Seq[Step]
}
object PremiseDerivationStep {

  case class Simple(inner: SimpleDerivation) extends PremiseDerivationStep {
    override def statement: Statement = inner.statement
    override def proofSteps: Seq[Step] = inner.toProofSteps
  }
  case class Wrapped(wrappers: Seq[Unwrapper], inner: SimpleDerivation) extends PremiseDerivationStep {
    override def statement: Statement = wrappers.addToStatement(inner.statement)
    override def proofSteps: Seq[Step] = Seq(Step.WrappedPremiseDerivationStep(wrappers, inner.toProofSteps))
  }
}
