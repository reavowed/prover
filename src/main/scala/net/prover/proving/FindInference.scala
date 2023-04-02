package net.prover.proving

import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.model.{AvailableEntries, Inference}

import scala.util.Try

object FindInference {
  def apply(inferenceId: String)(implicit availableEntries: AvailableEntries): Try[Inference.Summary] = {
    availableEntries.allInferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }
}
