package net.prover.model

case class Variables(statementVariables: Seq[StatementVariable], termVariables: Seq[TermVariable]) {
  def +(termVariable: TermVariable): Variables = {
    copy(termVariables = (termVariables :+ termVariable).distinct)
  }
  def ++(otherVariables: Variables): Variables = {
    Variables(
      (statementVariables ++ otherVariables.statementVariables).distinct,
      (termVariables ++ otherVariables.termVariables).distinct)
  }
}
