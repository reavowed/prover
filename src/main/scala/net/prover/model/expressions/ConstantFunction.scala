package net.prover.model.expressions

case class ConstantFunction(term: Term) extends Function {
  override def apply(otherTerm: Term) = term
  override def serialized = s"constant ${term.serialized}"
  override def toString = term.toString
  override def safeToString = term.safeToString
}
