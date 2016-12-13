package net.prover.model

trait StatementDefinition {
  def symbol: String
  def defaultStatement: Statement
  def definingStatement: Option[Statement]

  def forwardDeduction: Option[Deduction] = definingStatement.map { s =>
    new Deduction {
      override val id: String = s"apply-$symbol"
      override val premiseTemplates: Seq[Statement] = Seq(s)
      override val conclusionTemplate: Statement = defaultStatement
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariableRequirements: DistinctVariableRequirements = DistinctVariableRequirements.empty
    }
  }

  def reverseDeduction: Option[Deduction] = definingStatement.map { s =>
    new Deduction {
      override val id: String = s"unapply-$symbol"
      override val premiseTemplates: Seq[Statement] = Seq(defaultStatement)
      override val conclusionTemplate: Statement = s
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariableRequirements: DistinctVariableRequirements = DistinctVariableRequirements.empty
    }
  }
}

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
    val (fromStatement, toStatement) =
        definedStatement.attemptMatch(statement).map(_ => (definedStatement, definingStatement))
          .orElse(definingStatement.attemptMatch(statement).map(_ => (definingStatement, definedStatement)))
          .getOrElse(throw new Exception(s"Could not apply definition to statement '$statement'"))
    matchPremisesToConclusion(
      Seq((statement, fromStatement)),
      toStatement,
      line,
      context)
  }
}
