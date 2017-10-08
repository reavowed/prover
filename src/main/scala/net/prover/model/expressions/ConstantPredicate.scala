package net.prover.model.expressions

import net.prover.model.Substitutions

case class ConstantPredicate(statement: Assertable, depth: Int) extends Predicate {
  override def apply(arguments: Seq[Objectable]) = {
    if (depth == 1)
      statement
    else
      ConstantPredicate(statement, depth - 1)
  }
  override def increaseDepth(additionalDepth: Int) = {
    ConstantPredicate(statement, depth + additionalDepth)
  }

  override def requiredSubstitutions = statement.requiredSubstitutions
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case ConstantPredicate(otherStatement, `depth`) =>
        statement.calculateSubstitutions(otherStatement, substitutions)
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    for {
      substitutedStatement <- statement.applySubstitutions(substitutions)
    } yield copy(substitutedStatement)
  }
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    Seq(ConstantPredicate(statement, depth + 1) -> substitutions)
  }
  override def replacePlaceholder(other: Expression) = copy(statement.replacePlaceholder(other))

  override def serialized = statement.serialized
  override def toString = statement.toString
  override def safeToString = statement.safeToString
}
