package net.prover.model.components

import net.prover.model.{DistinctVariables, PartialSubstitutions, Substitutions}

trait Placeholder[T <: Component] extends Component {
  override def allVariables: Set[Variable] = Set.empty
  override def presentVariables: Set[Variable] = Set.empty
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set.empty)
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  override def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = Set.empty
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ) = {
    throw new Exception("Cannot calculate substitutions for placeholder")
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    throw new Exception("Cannot apply substitutions to placeholder")
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[T] = {
    throw new Exception("Cannot make substitution into placeholder")
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ) = {
    throw new Exception("Cannot resolve substitution for placeholder")
  }
  override def validateSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, target: Component, distinctVariables: DistinctVariables) = {
    throw new Exception("Cannot validate substitution for placeholder")
  }
  override def findSubstitution(target: Component, termVariableToBeReplaced: TermVariable) = {
    throw new Exception("Cannot find substitution for placeholder")
  }
  override def findDoubleSubstitution(
    target: Component,
    firstTermVariable: TermVariable,
    firstTerm: Term,
    secondTermVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    throw new Exception("Cannot find double substitution for placeholder")
  }
  override def html: String = "???"
  override def serialized: String = "_"
}
