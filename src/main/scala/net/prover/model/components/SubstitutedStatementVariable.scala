package net.prover.model.components

case class SubstitutedStatementVariable(
    variable: StatementVariable,
    firstTerm: Term,
    firstVariable: TermVariable,
    tailReplacements: Seq[(Term, TermVariable)])
  extends Statement with SubstitutedVariable[Statement, StatementVariable]
{
  override val boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = {
    DefinedComponent.tryBoundAndFree(allReplacements.map(_._1), Set.empty).get
  }
  override def update(updatedReplacements: Seq[(Term, TermVariable)]): Statement = {
    updatedReplacements match {
      case (updatedFirstTerm, updatedFirstVariable) +: updatedTailReplacements =>
        SubstitutedStatementVariable(variable, updatedFirstTerm, updatedFirstVariable, updatedTailReplacements)
      case Nil =>
        variable
    }
  }
}
