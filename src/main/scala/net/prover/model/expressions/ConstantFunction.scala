package net.prover.model.expressions

import net.prover.model.Substitutions

case class ConstantFunction(term: Term) extends Function {
  override def apply(otherTerm: Term) = term

  override def boundVariables = term.boundVariables
  override def requiredSubstitutions = term.requiredSubstitutions
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case ConstantFunction(otherTerm) =>
        term.calculateSubstitutions(otherTerm, substitutions, boundVariableCount)
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    for {
      substitutedTerm <- term.applySubstitutions(substitutions)
    } yield ConstantFunction(substitutedTerm)
  }
  override def replacePlaceholder(other: Expression) = copy(term.replacePlaceholder(other))

  override def serialized = s"constant ${term.serialized}"
  override def toString = term.toString
  override def safeToString = term.safeToString
}
