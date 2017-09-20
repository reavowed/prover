package net.prover.model.expressions

case object IdentityFunction extends Function {
  override def apply(term: Term): Term = term
  override def serialized: String = "identity"
  override def toString = "_"
  override def safeToString = "_"
}