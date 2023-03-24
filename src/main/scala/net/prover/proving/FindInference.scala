package net.prover.proving

import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.model.Inference
import net.prover.model.proof.StepProvingContext

import scala.util.Try

object FindInference {
  def apply(inferenceId: String)(implicit stepProvingContext: StepProvingContext): Try[Inference.Summary] = {
    stepProvingContext.provingContext.entryContext.allInferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }
}
