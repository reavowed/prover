package net.prover.model.expressions

trait ExpressionFunction[+T <: Expression] {
  def apply(term: Term): T
  def serialized: String
  def safeToString: String
}
