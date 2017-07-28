package net.prover.model.components

object PlaceholderStatement extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Component) = {
    Some(other.asInstanceOf[Statement])
  }
}
