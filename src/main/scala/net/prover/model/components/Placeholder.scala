package net.prover.model.components

import net.prover.model.Substitutions

trait Placeholder[T <: Component] extends Component {
  override def variables: Seq[Variable] = Nil
  override def calculateSubstitutions(
    other: Component,
    substitutions: Substitutions
  ) = {
    throw new Exception("Cannot calculate substitutions for placeholder")
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    throw new Exception("Cannot apply substitutions to placeholder")
  }
  override def toString: String = "_"
  override def serialized: String = "_"
}
