package net.prover.model.expressions

import net.prover.model.Substitutions

trait Placeholder[T <: Expression] extends Expression {
  override def requiredSubstitutions = Substitutions.Required.empty
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    throw new Exception("Cannot calculate substitutions for placeholder")
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    throw new Exception("Cannot apply substitutions to placeholder")
  }
  override def toString: String = "_"
  override def serialized: String = "_"
}
