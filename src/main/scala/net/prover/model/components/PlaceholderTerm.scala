package net.prover.model.components

object PlaceholderTerm extends Term with Placeholder[Term] {
  def replacePlaceholder(other: Component) = {
    Some(other.asInstanceOf[Term])
  }
}
