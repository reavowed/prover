package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepProvingContext}

trait PremiseSimplifier {
  def inference: Inference
  def getPremiseSimplification(premise: Statement)(implicit stepProvingContext: StepProvingContext): Option[(Statement, Step)]
}
