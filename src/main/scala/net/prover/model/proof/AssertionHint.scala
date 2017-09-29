package net.prover.model.proof

import net.prover.model.expressions.Assertable
import net.prover.model.{Inference, Substitutions}

case class AssertionHint(
  inference: Inference,
  conclusion: Assertable,
  substitutions: Substitutions)

object AssertionHint {
  def attempt(
    inferenceId: String,
    availableInferences: Seq[Inference],
    inferenceSubstitutions: Inference.Substitutions,
    conclusion: Assertable
  ): Option[AssertionHint] = {
    for {
      inference <- availableInferences.find(_.id == inferenceId)
      substitutions <- inference.generalizeSubstitutions(inferenceSubstitutions)
    } yield AssertionHint(inference, conclusion, substitutions)
  }
  def attempt(
    inferenceId: String,
    availableInferences: Seq[Inference],
    inferenceSubstitutions: Inference.Substitutions
  ): Option[AssertionHint] = {
    for {
      inference <- availableInferences.find(_.id == inferenceId)
      substitutions <- inference.generalizeSubstitutions(inferenceSubstitutions)
      conclusion <- inference.conclusion.applySubstitutions(substitutions)
    } yield AssertionHint(inference, conclusion, substitutions)
  }
}
