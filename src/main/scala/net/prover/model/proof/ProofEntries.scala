package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.entries.StatementDefinition

case class ProofEntries(
  availableInferences: Seq[Inference],
  statementDefinitions: Seq[StatementDefinition])
{
  def ++(other: ProofEntries) = ProofEntries(
    availableInferences ++ other.availableInferences,
    statementDefinitions ++ other.statementDefinitions)
}
