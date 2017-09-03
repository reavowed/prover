package net.prover.model.components

object PlaceholderTerm extends Term with Placeholder[Term] {
  override def replacePlaceholder(other: Component) = other.asInstanceOf[Term]
}
