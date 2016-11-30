package net.prover.model

trait Definition extends DirectStepParser {
  def definedStatement: Statement
  def definingStatement: Statement

  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (statement, lineAfterStatement) = readReference(line, theoremBuilder)
    applyToStatement(statement, lineAfterStatement, theoremBuilder, context).mapLeft(Step(_))
  }

  def applyToStatement(
    statement: Statement,
    line: PartialLine,
    theoremBuilder: TheoremBuilder,
    context: Context
  ): (Statement, PartialLine) = {
    val (fromStatementMatch, toStatement) =
        definedStatement.attemptMatch(statement).map((_, definingStatement))
          .orElse(definingStatement.attemptMatch(statement).map((_, definedStatement)))
          .getOrElse(throw new Exception(s"Could not apply definition to statement '$statement'"))
    val (matcher, remainingLine) = fromStatementMatch.expand(toStatement.variables, line, context)
    (toStatement.applyMatch(matcher), remainingLine)
  }
}
