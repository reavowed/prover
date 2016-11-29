package net.prover.model

trait Definition extends DirectStepParser {
  def definedStatement: Statement
  def definingStatement: Statement

  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (reference, lineAfterReference) = line.splitFirstWord
    val referredStatement = theoremBuilder.resolveReference(reference)
    val replacedStatement = applyToStatement(referredStatement)
    val step = Step(replacedStatement)
    (step, lineAfterReference)
  }

  def applyToStatement(statement: Statement): Statement = {
    definedStatement.attemptMatch(statement).map(definingStatement.replace)
      .orElse(definingStatement.attemptMatch(statement).map(definedStatement.replace))
      .getOrElse(throw new Exception(s"Could not apply definition to statement '$statement'"))
  }
}
