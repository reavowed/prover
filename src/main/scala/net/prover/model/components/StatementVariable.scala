package net.prover.model.components

import net.prover.model.{DistinctVariables, PartialSubstitutions, Substitutions}

import scala.collection.immutable.Nil

case class StatementVariable(text: String) extends Statement with Variable {
  def sub(termToReplaceWith: Term, termToBeReplaced: TermVariable): Statement = {
    SubstitutedStatementVariable(this, termToReplaceWith, termToBeReplaced)
  }

  override def allVariables: Set[Variable] = Set(this)
  override def presentVariables: Set[Variable] = Set(this)
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set.empty)
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = Set(this)
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = other match {
    case otherStatement: Statement =>
      substitutions.tryAdd(this, otherStatement).toSeq
    case _ =>
      Nil
  }
  def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Statement])
  }
  override def validateSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    if (target == this) {
      if (termToBeReplaced == termToReplaceWith)
        Some(DistinctVariables.empty)
      else
        Some(DistinctVariables(termToBeReplaced -> this))
    } else if (makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables).contains(target)) {
      Some(DistinctVariables.empty)
    } else {
      None
    }
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables) = {
    if (termToReplaceWith == termToBeReplaced)
      Some(this)
    else if (distinctVariables.areDistinct(termToBeReplaced, this))
      Some(this)
    else
      Some(sub(termToReplaceWith, termToBeReplaced))
  }
  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Statement, DistinctVariables)] = {
    if (this == other) {
      Some((this, DistinctVariables.empty))
    } else {
      None
    }
  }
  override def findSubstitution(
    target: Component,
    termVariableToBeReplaced: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (this == target) {
      (Seq((termVariableToBeReplaced, DistinctVariables.empty)), Some(DistinctVariables(termVariableToBeReplaced -> this)))
    } else {
      target match {
        case SubstitutedStatementVariable(variable, termToReplaceWith, `termVariableToBeReplaced`) if variable == this =>
          (Seq((termToReplaceWith, DistinctVariables.empty)), None)
        case _ =>
          (Nil, None)
      }
    }
  }
  override def findDoubleSubstitution(
    target: Component,
    firstTermVariable: TermVariable,
    firstTerm: Term,
    secondTermVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (firstTerm == firstTermVariable) {
      findSubstitution(target, secondTermVariable)
    } else {
      sub(firstTerm, firstTermVariable).findSubstitution(target, secondTermVariable)
    }
  }
  override def replacePlaceholder(other: Component) = Some(this)

  override def html: String = text
  override def serialized: String = text
}
