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
  def filter(f: StatementVariable => Boolean, g: TermVariable => Boolean): Variables = {
    copy(statementVariables = statementVariables.filter(f), termVariables = termVariables.filter(g))
  }
  def isEmpty: Boolean = statementVariables.isEmpty && termVariables.isEmpty
  def nonEmpty: Boolean = statementVariables.nonEmpty || termVariables.nonEmpty
}

object Variables {
  val empty = Variables(Set.empty, Set.empty)
}
