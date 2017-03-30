package net.prover.model

case class ProvenStatement(
    statement: Statement,
    conditions: Conditions)
{
  def applySubstitutions(substitutions: Substitutions): Option[ProvenStatement] = {
    for {
      updatedStatement <- statement.applySubstitutions(substitutions)
      updateConditions <- conditions.applySubstitutions(substitutions)
    } yield ProvenStatement(updatedStatement, updateConditions)
  }
}

object ProvenStatement {
  def withNoConditions(statement: Statement): ProvenStatement = {
    ProvenStatement(statement, Conditions.empty)
  }
}
