package net.prover.model.components

case class SubstitutedStatementVariable(
    variable: StatementVariable,
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable)
 extends Statement with SubstitutedVariable[Statement, StatementVariable]
{
  override def update(
    updatedVariable: StatementVariable,
    updatedTermToReplaceWith: Term,
    updatedTermToBeReplaced: TermVariable
  ): SubstitutedStatementVariable = {
    SubstitutedStatementVariable(updatedVariable, updatedTermToReplaceWith, updatedTermToBeReplaced)
  }
  override def html: String = "[" + termToReplaceWith.safeHtml + "/" + termToBeReplaced.html + "]" + variable.html
  override def serialized: String = Seq(
    "sub",
    termToReplaceWith.serialized,
    termToBeReplaced.serialized,
    variable.serialized
  ).mkString(" ")
}
