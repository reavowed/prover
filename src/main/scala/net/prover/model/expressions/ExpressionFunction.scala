package net.prover.model.expressions

import net.prover.model.Substitutions

trait ExpressionFunction[+T <: Expression] extends Expression {
  def apply(arguments: Seq[Objectable]): T
  def increaseDepth(additionalDepth: Int): ExpressionFunction[T]

  def applySubstitutions(substitutions: Substitutions): Option[ExpressionFunction[T]]
  def replacePlaceholder(other: Expression): ExpressionFunction[T]

  override def makeApplicative = None
}
