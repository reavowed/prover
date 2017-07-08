package net.prover.model.components

import net.prover.model.{DistinctVariables, Html, PartialSubstitutions, Substitutions}

import scala.collection.immutable.Nil

case class TermVariable(text: String) extends Term with Variable {
  override def allVariables: Set[Variable] = Set(this)
  override def presentVariables: Set[Variable] = Set(this)
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set(this))
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  override def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = Set(this)
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = {
    other match {
      case otherTerm: Term =>
        substitutions.tryAdd(this, otherTerm).toSeq
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Term])
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Term] = {
    if (termToBeReplaced == this)
      Some(termToReplaceWith)
    else if (termToBeReplaced == termToReplaceWith)
      Some(this)
    else if (distinctVariables.areDistinct(this, termToBeReplaced))
      Some(this)
    else
      Some(SubstitutedTermVariable(this, termToReplaceWith, termToBeReplaced))
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
  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Term, DistinctVariables)] = {
    if (this == thisTerm && other == otherTerm) {
      Some((termVariable, DistinctVariables.empty))
    } else if (this == other && this != termVariable) {
      Some((this, DistinctVariables(termVariable -> this)))
    } else {
      None
    }
  }
  override def findSubstitution(
    target: Component,
    termVariableToBeReplaced: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (this == termVariableToBeReplaced) {
      (Seq((target.asInstanceOf[Term], DistinctVariables.empty)), None)
    } else if (this == target) {
      (Seq((termVariableToBeReplaced, DistinctVariables.empty)), Some(DistinctVariables(termVariableToBeReplaced -> this)))
    }  else {
      target match {
        case SubstitutedTermVariable(variable, termToReplaceWith, `termVariableToBeReplaced`) if variable == this =>
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
    if (firstTermVariable == firstTerm) {
      this.findSubstitution(target, secondTermVariable)
    } else if (this == firstTermVariable) {
      firstTerm.findSubstitution(target, secondTermVariable)
    } else {
      val (termsStraight, noTermStraight) = this.findSubstitution(target, secondTermVariable)
      val (termsSubbed, noTermSubbed) = SubstitutedTermVariable(this, firstTerm, firstTermVariable).findSubstitution(target, secondTermVariable)
      val jointTerms = termsStraight.map(_.mapRight(_ ++ DistinctVariables(firstTermVariable -> this))) ++ termsSubbed
      (jointTerms, noTermStraight.map(_ ++ DistinctVariables(firstTermVariable -> this)) orElse noTermSubbed)
    }
  }
  override def condenseWithSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    def tryWith(thisTarget: Term, additionalDistinctVariables: DistinctVariables): Option[(PartialSubstitutions, PartialSubstitutions)] = {
      other.calculateSubstitutions(thisTarget, otherSubstitutions)
        .map(thisSubstitutions.withDistinctVariables(additionalDistinctVariables) -> _)
        .headOption
    }
    super.condenseWithSubstitution(termToReplaceWith, termToBeReplaced, other, thisSubstitutions, otherSubstitutions) orElse
      (if (termToBeReplaced == termToReplaceWith) {
        tryWith(this, DistinctVariables.empty)
      } else if (this == termToBeReplaced) {
        tryWith(termToReplaceWith, DistinctVariables.empty)
      } else {
        tryWith(this, DistinctVariables(termToBeReplaced -> this)) orElse
          tryWith(SubstitutedTermVariable(this, termToReplaceWith, termToBeReplaced), DistinctVariables.empty)
      })
  }
  override def replacePlaceholder(other: Component) = Some(this)
  override def html: String = Html.format(text)
  override def serialized: String = text
}
