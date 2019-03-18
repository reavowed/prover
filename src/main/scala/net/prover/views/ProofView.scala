package net.prover.views

import net.prover.model.DisplayContext
import net.prover.model.proof.Step

import scala.xml.Elem

object ProofView {
  def apply(proof: Seq[Step])(implicit displayContext: DisplayContext): Elem = {
    <div class="proof">
      {proof.flatMapWithIndex((s, i) => StepView(s, Seq(i)))}
    </div>
  }
}
