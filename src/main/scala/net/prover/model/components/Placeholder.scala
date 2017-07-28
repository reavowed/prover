package net.prover.model.components

import net.prover.model.Substitutions

trait Placeholder[T <: Component] extends Component {
  override def variables: Set[Variable] = Set.empty
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set.empty)
  override def calculateSubstitutions(
    other: Component,
    substitutions: Substitutions
  ) = {
    throw new Exception("Cannot calculate substitutions for placeholder")
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    throw new Exception("Cannot apply substitutions to placeholder")
  }
  override def html: String = "???"
  override def toString: String = "_"
  override def serialized: String = "_"
}
