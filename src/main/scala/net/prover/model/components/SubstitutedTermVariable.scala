package net.prover.model.components

case class SubstitutedTermVariable(
    variable: TermVariable,
    firstTerm: Term,
    firstVariable: TermVariable,
    tailReplacements: Seq[(Term, TermVariable)])
  extends Term with SubstitutedVariable[Term, TermVariable]
{
  override val boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = {
    val (replacementBound, replacementFree) = DefinedComponent.tryBoundAndFree(allReplacements.map(_._1), Set.empty).get
    (replacementBound, replacementFree + variable)
  }
  override def update(updatedReplacements: Seq[(Term, TermVariable)]): Term = {
    updatedReplacements match {
      case (updatedFirstTerm, updatedFirstVariable) +: updatedTailReplacements =>
        SubstitutedTermVariable(variable, updatedFirstTerm, updatedFirstVariable, updatedTailReplacements)
      case Nil =>
        variable
    }
  }
}
