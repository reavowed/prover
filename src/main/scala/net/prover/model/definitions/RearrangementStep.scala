package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Expression
import net.prover.model.proof.Step

case class RearrangementStep[TComponent <: Expression](result: TComponent, substeps: Seq[Step], elider: Seq[Step] => Option[Step]) {
  def elidedStep: Option[Step] = elider(substeps)
}

object RearrangementStep {
  def apply[TComponent <: Expression](result: TComponent, substeps: Seq[Step], inference: Inference.Summary): RearrangementStep[TComponent] = {
    RearrangementStep(result, substeps, Step.Elided.ifNecessary(_, inference))
  }
  def apply[TComponent <: Expression](result: TComponent, substeps: Seq[Step], description: String): RearrangementStep[TComponent] = {
    RearrangementStep(result, substeps, Step.Elided.ifNecessary(_, description))
  }
  def apply[TComponent <: Expression](result: TComponent, substeps: Seq[Step], inference: Option[Inference.Summary], description: String): RearrangementStep[TComponent] = {
    RearrangementStep(result, substeps, Step.Elided.ifNecessary(_, inference, description))
  }
}
