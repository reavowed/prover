package net.prover.model.components

object PlaceholderStatement extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Component) = other.asInstanceOf[Statement]
}
