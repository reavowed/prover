package net.prover.model.components

import net.prover.model.Substitutions

object PlaceholderStatement extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Component) = other.asInstanceOf[Statement]
  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    Seq((Predicate.Constant(this), substitutions))
  }
}
