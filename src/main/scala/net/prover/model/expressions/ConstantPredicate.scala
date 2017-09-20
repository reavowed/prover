package net.prover.model.expressions

case class ConstantPredicate(statement: Statement) extends Predicate {
  override def apply(term: Term) = statement
  override def serialized = s"constant ${statement.serialized}"
  override def toString = statement.toString
  override def safeToString = statement.safeToString
}
