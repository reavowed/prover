package net.prover.model

case class Substitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term]) {

  def +(v: StatementVariable, s: Statement): Substitutions = {
    copy(statements = statements + (v -> s))
  }

  def +(v: TermVariable, s: Term): Substitutions = {
    copy(terms = terms + (v -> s))
  }
}

object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty, Map.empty)
}
