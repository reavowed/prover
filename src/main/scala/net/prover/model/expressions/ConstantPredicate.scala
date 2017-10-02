package net.prover.model.expressions

import net.prover.model.Substitutions

case class ConstantPredicate(statement: Statement) extends Predicate {
  override def apply(arguments: Seq[Term]) = statement

  override def boundVariables = statement.boundVariables
  override def requiredSubstitutions = statement.requiredSubstitutions
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case ConstantPredicate(otherStatement) =>
        statement.calculateSubstitutions(otherStatement, substitutions, boundVariableCount)
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    for {
      substitutedStatement <- statement.applySubstitutions(substitutions)
    } yield copy(substitutedStatement)
  }
  override def replacePlaceholder(other: Expression) = copy(statement.replacePlaceholder(other))

  override def serialized = s"constant ${statement.serialized}"
  override def toString = statement.toString
  override def safeToString = statement.safeToString
}
