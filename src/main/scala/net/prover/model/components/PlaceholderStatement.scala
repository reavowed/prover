package net.prover.model.components

import net.prover.model.DistinctVariables

object PlaceholderStatement extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Component) = {
    Some(other.asInstanceOf[Statement])
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Statement] = {
    Some(SubstitutedPlaceholderStatement(termToReplaceWith, termToBeReplaced))
  }
}
