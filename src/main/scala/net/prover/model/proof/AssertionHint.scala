package net.prover.model.proof

import net.prover.model.expressions.Statement
import net.prover.model.{Inference, Substitutions}

case class AssertionHint(
  inference: Inference,
  conclusion: Statement,
  substitutions: Substitutions)

object AssertionHint {
  def attempt(
    inferenceId: String,
    availableInferences: Seq[Inference],
    inferenceSubstitutions: Inference.Substitutions,
    conclusion: Statement,
    depth: Int
  ): Option[AssertionHint] = {
    for {
      inference <- availableInferences.find(_.id == inferenceId)
      substitutions <- inference.generalizeSubstitutions(inferenceSubstitutions, depth)
    } yield AssertionHint(inference, conclusion, substitutions)
  }
  def attempt(
    inferenceId: String,
    availableInferences: Seq[Inference],
    inferenceSubstitutions: Inference.Substitutions,
    depth: Int
  ): Option[AssertionHint] = {
    for {
      inference <- availableInferences.find(_.id == inferenceId)
      substitutions <- inference.generalizeSubstitutions(inferenceSubstitutions, depth)
      conclusion <- inference.conclusion.applySubstitutions(substitutions)
    } yield AssertionHint(inference, conclusion, substitutions)
  }
}
