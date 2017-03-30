package net.prover.model

case class Variables(statementVariables: Set[StatementVariable], termVariables: Set[TermVariable]) {
  def +(termVariable: TermVariable): Variables = {
    copy(termVariables = termVariables + termVariable)
  }
  def +(statementVariable: StatementVariable): Variables = {
    copy(statementVariables = statementVariables + statementVariable)
  }
  def -(termVariable: TermVariable): Variables = {
    copy(termVariables = termVariables - termVariable)
  }
  def ++(otherVariables: Variables): Variables = {
    Variables(
      statementVariables ++ otherVariables.statementVariables,
      termVariables ++ otherVariables.termVariables)
  }
  def intersect(otherVariables: Variables): Variables = {
    Variables(
      statementVariables.intersect(otherVariables.statementVariables),
      termVariables.intersect(otherVariables.termVariables))
  }
  def nonEmpty: Boolean = statementVariables.nonEmpty || termVariables.nonEmpty
}

object Variables {
  val empty = Variables(Set.empty, Set.empty)
}
