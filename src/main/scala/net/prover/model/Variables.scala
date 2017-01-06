package net.prover.model

case class Variables(statementVariables: Seq[StatementVariable], termVariables: Seq[TermVariable]) {
  def +:(termVariable: TermVariable): Variables = {
    copy(termVariables = (termVariable +: termVariables).distinct)
  }
  def :+(statementVariable: StatementVariable): Variables = {
    copy(statementVariables = (statementVariables :+ statementVariable).distinct)
  }
  def -(termVariable: TermVariable): Variables = {
    copy(termVariables = termVariables.filter(_ != termVariable))
  }
  def ++(otherVariables: Variables): Variables = {
    Variables(
      (statementVariables ++ otherVariables.statementVariables).distinct,
      (termVariables ++ otherVariables.termVariables).distinct)
  }
}

object Variables {
  val empty = Variables(Nil, Nil)
}
