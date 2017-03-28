package net.prover.model

case class ProvenStatement(
    statement: Statement,
    conditions: Conditions)
{
  def applySubstitutions(substitutions: Substitutions): ProvenStatement = {
    ProvenStatement(
      statement.applySubstitutions(substitutions),
      conditions.applySubstitutions(substitutions))
  }
}

object ProvenStatement {
  def withNoConditions(statement: Statement): ProvenStatement = {
    ProvenStatement(statement, Conditions.empty)
  }
}
