package net.prover.model

case class ProvenStatement(
    statement: Statement,
    arbitraryVariables: Seq[TermVariable],
    distinctVariables: DistinctVariables)
{
  def applySubstitutions(substitutions: Substitutions): ProvenStatement = {
    ProvenStatement(
      statement.applySubstitutions(substitutions),
      arbitraryVariables
        .map(_.applySubstitutions(substitutions))
        .map(Term.asVariable),
      distinctVariables.applySubstitutions(substitutions))
  }
}

object ProvenStatement {
  def withNoConditions(statement: Statement): ProvenStatement = {
    ProvenStatement(statement, Nil, DistinctVariables.empty)
  }
}
