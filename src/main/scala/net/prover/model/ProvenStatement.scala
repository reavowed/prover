package net.prover.model

case class ProvenStatement(
    statement: Statement,
    conditions: Conditions)
{
  def applySubstitutions(substitutions: Substitutions, distinctVariables: Map[TermVariable, Variables]): Option[ProvenStatement] = {
    for {
      updatedStatement <- statement.applySubstitutions(substitutions, distinctVariables)
      updateConditions <- conditions.applySubstitutions(substitutions)
    } yield ProvenStatement(updatedStatement, updateConditions)
  }
}

object ProvenStatement {
  def withNoConditions(statement: Statement): ProvenStatement = {
    ProvenStatement(statement, Conditions.empty)
  }
}
