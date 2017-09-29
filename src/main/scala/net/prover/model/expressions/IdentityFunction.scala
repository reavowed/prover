package net.prover.model.expressions

import net.prover.model.Substitutions

case object IdentityFunction extends Function {
  override def apply(term: Term): Term = term

  override def boundVariables = Set.empty
  override def requiredSubstitutions = Substitutions.Required.empty
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = Seq(substitutions)
  override def applySubstitutions(substitutions: Substitutions) = Some(this)
  override def replacePlaceholder(other: Expression) = this

  override def serialized: String = "identity"
  override def toString = "_"
  override def safeToString = "_"
}