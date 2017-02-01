package net.prover.model

case class Context(
    statementParsers: Seq[StatementParser],
    termSpecifications: Seq[TermSpecification],
    theoremLineParsers: Seq[TheoremLineParser],
    variables: Variables) {

  def combine(others: Seq[Context]): Context = {
    Context(
      others.flatMap(_.statementParsers) ++ statementParsers,
      others.flatMap(_.termSpecifications) ++ termSpecifications,
      others.flatMap(_.theoremLineParsers) ++ theoremLineParsers,
      variables
    )
  }

  def inferences: Seq[Inference] = theoremLineParsers.ofType[Inference]

  def addStatementDefinition(statementDefinition: StatementDefinition): Context = {
    copy(
      statementParsers = statementParsers :+ statementDefinition,
      theoremLineParsers = theoremLineParsers ++
        statementDefinition.forwardInference.toSeq ++
        statementDefinition.reverseInference.toSeq)
  }

  def addTermDefinition(termDefinition: TermDefinition) = {
    copy(
      termSpecifications = termSpecifications :+ termDefinition.specification,
      theoremLineParsers = theoremLineParsers :+ termDefinition.inference)
  }
}

object Context {
  val empty = Context(Nil, Nil, Nil, Variables.empty)
}
