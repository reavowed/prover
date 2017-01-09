package net.prover.model

trait StatementDefinition {
  def symbol: String
  def defaultStatement: Statement
  def definingStatement: Option[Statement]
  def distinctVariables: DistinctVariables

  def forwardDeduction: Option[Deduction] = definingStatement.map { s =>
    new Deduction {
      override val id: String = s"apply-$symbol"
      override val premiseTemplates: Seq[Statement] = Seq(s)
      override val conclusionTemplate: Statement = defaultStatement
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariables: DistinctVariables =
        StatementDefinition.this.distinctVariables
    }
  }

  def reverseDeduction: Option[Deduction] = definingStatement.map { s =>
    new Deduction {
      override val id: String = s"unapply-$symbol"
      override val premiseTemplates: Seq[Statement] = Seq(defaultStatement)
      override val conclusionTemplate: Statement = s
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariables: DistinctVariables =
        StatementDefinition.this.distinctVariables
    }
  }
}
