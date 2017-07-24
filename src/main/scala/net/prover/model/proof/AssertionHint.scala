package net.prover.model.proof

import net.prover.model.{Inference, ProvenStatement, Substitutions}

case class AssertionHint(
  inference: Inference,
  conclusion: ProvenStatement,
  substitutions: Substitutions)

object AssertionHint {
  def attempt(
    inferenceSummary: Inference.Summary,
    availableInferences: Seq[Inference],
    substitutions: Substitutions,
    conclusion: ProvenStatement
  ): Option[AssertionHint] = {
    for {
      inference <- availableInferences.find(_.id == inferenceSummary.id)
    } yield AssertionHint(inference, conclusion, substitutions)
  }
  def attempt(
    inferenceSummary: Inference.Summary,
    availableInferences: Seq[Inference],
    substitutions: Substitutions
  ): Option[AssertionHint] = {
    for {
      inference <- availableInferences.find(_.id == inferenceSummary.id)
      conclusion <- inference.conclusion.applySubstitutions(substitutions)
    } yield AssertionHint(inference, conclusion, substitutions)
  }
}
