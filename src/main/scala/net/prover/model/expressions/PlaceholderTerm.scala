package net.prover.model.expressions

object PlaceholderTerm extends Term with Placeholder[Term] {
  override def replacePlaceholder(other: Expression) = other.asInstanceOf[Term]
}
