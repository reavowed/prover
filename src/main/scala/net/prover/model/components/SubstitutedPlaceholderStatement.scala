package net.prover.model.components

import net.prover.model.DistinctVariables

case class SubstitutedPlaceholderStatement(
  termToReplaceWith: Term,
  termToBeReplaced: TermVariable)
  extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Component): Option[Statement] = {
    other.asInstanceOf[Statement].makeSingleSubstitution(termToReplaceWith, termToBeReplaced, DistinctVariables.empty)
  }
}
