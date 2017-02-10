package net.prover.model

case class Context(
    statementDefinitions: Seq[StatementDefinition],
    termSpecifications: Seq[TermSpecification],
    theoremLineParsers: Seq[TheoremLineParser],
    variables: Variables) {

  def combine(others: Seq[Context]): Context = {
    Context(
      others.flatMap(_.statementDefinitions) ++ statementDefinitions,
      others.flatMap(_.termSpecifications) ++ termSpecifications,
      others.flatMap(_.theoremLineParsers) ++ theoremLineParsers,
      variables
    )
  }

  def inferences: Seq[Inference] = theoremLineParsers.ofType[Inference]

  def addStatementDefinition(statementDefinition: StatementDefinition): Context = {
    copy(
      statementDefinitions = statementDefinitions :+ statementDefinition,
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
