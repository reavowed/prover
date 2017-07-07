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

  def serialized: String = {
    if (conditions.isEmpty)
      statement.serialized
    else
      s"${statement.serialized} ${conditions.serialized}"
  }
}

object ProvenStatement {
  def withNoConditions(statement: Statement): ProvenStatement = {
    ProvenStatement(statement, Conditions.empty)
  }
}
