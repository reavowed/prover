package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Term
import net.prover.model.proof.Step

case class RearrangementStep(resultingTerm: Term, substeps: Seq[Step], elider: Seq[Step] => Option[Step]) {
  def elidedStep: Option[Step] = elider(substeps)
}

object RearrangementStep {
  def apply(resultingTerm: Term, substeps: Seq[Step], inference: Inference.Summary): RearrangementStep = {
    RearrangementStep(resultingTerm, substeps, Step.Elided.ifNecessary(_, inference))
  }
  def apply(resultingTerm: Term, substeps: Seq[Step], description: String): RearrangementStep = {
    RearrangementStep(resultingTerm, substeps, Step.Elided.ifNecessary(_, description))
  }
  def apply(resultingTerm: Term, substeps: Seq[Step], inference: Option[Inference.Summary], description: String): RearrangementStep = {
    RearrangementStep(resultingTerm, substeps, Step.Elided.ifNecessary(_, inference, description))
  }
}
