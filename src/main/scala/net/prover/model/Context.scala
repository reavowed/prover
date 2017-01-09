package net.prover.model

case class Context(
    statementDefinitions: Seq[StatementDefinition],
    termParsers: Seq[TermParser],
    otherTheoremLineParsers: Seq[TheoremLineParser]) {

  def theoremLineParsers: Seq[TheoremLineParser] =
    statementDefinitions.flatMap(_.forwardDeduction) ++
    statementDefinitions.flatMap(_.reverseDeduction) ++
    otherTheoremLineParsers

  def +(other: Context): Context = {
    Context(
      statementDefinitions ++ other.statementDefinitions,
      termParsers ++ other.termParsers,
      otherTheoremLineParsers ++ other.otherTheoremLineParsers)
  }

  def deductions: Seq[Deduction] = theoremLineParsers.ofType[Deduction]
}

object Context {
  val empty = Context(Nil, Nil, Nil)
}
