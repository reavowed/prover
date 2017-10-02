package net.prover.model.expressions

import net.prover.model.Substitutions

trait ExpressionFunction[+T <: Expression] extends Expression {
  def apply(arguments: Seq[Term]): T

  def applySubstitutions(substitutions: Substitutions): Option[ExpressionFunction[T]]
  def replacePlaceholder(other: Expression): ExpressionFunction[T]

  override def calculateApplicatives(arguments: Seq[Term], substitutions: Substitutions, boundVariableCount: Int) = ???
  override def makeApplicative(argument: Term) = ???
}
