package net.prover.model

case class Context(
    statementParsers: Seq[StatementParser],
    termSpecifications: Seq[TermSpecification[_]],
    theoremLineParsers: Seq[TheoremLineParser]) {

  def +(other: Context): Context = {
    Context(
      statementParsers ++ other.statementParsers,
      termSpecifications ++ other.termSpecifications,
      theoremLineParsers ++ other.theoremLineParsers)
  }

  def deductions: Seq[Deduction] = theoremLineParsers.ofType[Deduction]

  def addStatementDefinition(statementDefinition: StatementDefinition): Context = {
    copy(
      statementParsers = statementParsers :+ statementDefinition,
      theoremLineParsers = theoremLineParsers ++ statementDefinition.forwardDeduction.toSeq ++ statementDefinition.reverseDeduction.toSeq)
  }
}

object Context {
  val empty = Context(Nil, Nil, Nil)
}
