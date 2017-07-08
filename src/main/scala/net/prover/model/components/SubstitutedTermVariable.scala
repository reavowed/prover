package net.prover.model.components

case class SubstitutedTermVariable(
  variable: TermVariable,
  termToReplaceWith: Term,
  termToBeReplaced: TermVariable)
  extends Term with SubstitutedVariable[Term, TermVariable]
{
  override def update(
    updatedVariable: TermVariable,
    updatedTermToReplaceWith: Term,
    updatedTermToBeReplaced: TermVariable
  ): SubstitutedTermVariable = {
    SubstitutedTermVariable(updatedVariable, updatedTermToReplaceWith, updatedTermToBeReplaced)
  }
}
