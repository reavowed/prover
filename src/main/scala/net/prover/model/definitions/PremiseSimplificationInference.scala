package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.model.proof.{PremiseStep, StepProvingContext}

trait PremiseSimplificationInference {
  def inference: Inference
  def getPremiseSimplification(premiseToMatch: Statement, existingPremises: Seq[(Statement, Seq[PremiseStep])])(implicit stepProvingContext: StepProvingContext): Option[(Statement, Seq[PremiseStep])]
}
