package net.prover.model.components

import net.prover.model.{DistinctVariables, PartialSubstitutions, Substitutions}

import scala.collection.immutable.Nil

trait SubstitutedVariable[+T <: Component, TVariable <: Variable] extends Component {
  def sub(termToReplaceWith: Term, termToBeReplaced: TermVariable): T = {
    update((termToReplaceWith, termToBeReplaced) +: allReplacements)
  }

  val variable: TVariable
  val firstTerm: Term
  val firstVariable: TermVariable
  val tailReplacements: Seq[(Term, TermVariable)]
  def tail: T = update(tailReplacements)
  def allReplacements: Seq[(Term, TermVariable)] = (firstTerm, firstVariable) +: tailReplacements

  def update(updatedReplacements: Seq[(Term, TermVariable)]): T

  override def allVariables: Set[Variable] = allReplacements.flatMap { case (term, termVariable) =>
    term.allVariables + termVariable
  }.toSet + variable
  override def presentVariables: Set[Variable] = allReplacements.flatMap { case (term, termVariable) =>
    term.presentVariables
  }.toSet + variable
  override def implicitDistinctVariables: DistinctVariables = allReplacements.map(_._1.implicitDistinctVariables).foldTogether
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = {
    allReplacements.foldRight(Set[Variable](variable)) { case ((rTerm, rTermVariable), variables) =>
      if(rTermVariable == termVariable)
        rTerm.getPotentiallyIntersectingVariables(termVariable)
      else
        variables ++ rTerm.getPotentiallyIntersectingVariables(termVariable)
    }
  }
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = {
    other match {
      case SubstitutedVariable(otherTerm, otherTermVariable, otherTail) =>
        (for {
          s1 <- tail.calculateSubstitutions(otherTail, substitutions)
          s2 <- firstTerm.calculateSubstitutions(otherTerm, s1)
          s3 <- firstVariable.calculateSubstitutions(otherTermVariable, s2)
        } yield s3) ++ substitutions.tryAdd(this, other)
      case _ =>
        substitutions.tryAdd(this, other)
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[T] = {
    for {
      updatedTail <- tail.applySubstitutions(substitutions)
      updatedTerm <- firstTerm.applySubstitutions(substitutions)
      updatedTermVariable <- firstVariable.applySubstitutions(substitutions).flatMap(Term.optionAsVariable)
      updated <- updatedTail.makeSingleSubstitution(updatedTerm, updatedTermVariable, substitutions.distinctVariables)
    } yield updated.asInstanceOf[T]
  }
  override def makeSingleSubstitution(
    newTermToReplaceWith: Term,
    newTermToBeReplaced: TermVariable,
    distinctVariables: DistinctVariables
  ): Option[T] = {
    def isSubstitutionAlreadyMade: Boolean = {
      def helper(remainingReplacements: Seq[(Term, TermVariable)]): Boolean = {
        remainingReplacements match {
          case (term, termVariable) +: replacementsTail if distinctVariables.areDistinct(newTermToBeReplaced, term) =>
            if (termVariable == newTermToBeReplaced)
              true
            else
              helper(replacementsTail)
          case Nil if distinctVariables.areDistinct(newTermToBeReplaced, variable) =>
            true
          case _ =>
            false
        }
      }
      helper(allReplacements)
    }
    def trySimplify: Option[T] = {
      def helper(remainingReplacements: Seq[(Term, TermVariable)]): Option[Seq[(Term, TermVariable)]] = {
        remainingReplacements match {
          case (`newTermToBeReplaced`, termVariable) +: replacementsTail
            if distinctVariables.areDistinct(newTermToBeReplaced, update(replacementsTail))
          =>
            if (newTermToReplaceWith == termVariable)
            // substituting [x/y] into [y/x]φ with (y,φ) distinct is just φ
              Some(replacementsTail)
            else
            // substituting [z/y] into [y/x]φ with (y,φ) distinct is [z/x]φ
              Some((newTermToReplaceWith, termVariable) +: replacementsTail)
          case (term, termVariable) +: replacementsTail
            if distinctVariables.areDistinct(newTermToBeReplaced, term) &&
              distinctVariables.areDistinct(newTermToBeReplaced, termVariable) &&
              distinctVariables.areDistinct(termVariable, newTermToReplaceWith)
          =>
            helper(replacementsTail).map((term, termVariable) +: _)
          case _ =>
            None
        }
      }
      helper(allReplacements).map(update)
    }

    if (newTermToReplaceWith == newTermToBeReplaced || isSubstitutionAlreadyMade)
      Some(this.asInstanceOf[T])
    else
      trySimplify orElse Some(update((newTermToReplaceWith, newTermToBeReplaced) +: allReplacements))
  }
  override def validateSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    target match {
      case SubstitutedVariable(`termToReplaceWith`, `firstVariable`, otherTail) if otherTail == tail && termToBeReplaced == firstTerm =>
        // Validating [x/y] into [y/z]φ to make [x/z]φ
        DistinctVariables.attempt(termToBeReplaced -> tail)
      case _ =>
        if (makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables).contains(target)) {
          Some(DistinctVariables.empty)
        } else {
          None
        }
    }
  }
  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(T, DistinctVariables)] = {
    if (other == this) {
      DistinctVariables.attempt(termVariable -> this).map(this.asInstanceOf[T] -> _)
    } else {
      other match {
        case SubstitutedVariable(`otherTerm`, `firstVariable`, otherTail) if tail == otherTail && firstTerm == thisTerm =>
          DistinctVariables.attempt(termVariable -> tail).map(update((termVariable, firstVariable) +: tailReplacements) -> _)
        case SubstitutedVariable(`firstTerm`, `firstVariable`, SubstitutedVariable(`otherTerm`, innerVariable, innerTail)) =>
          for {
            validationDistinctVariables <- innerTail.validateSingleSubstitution(thisTerm, innerVariable, tail, DistinctVariables.empty)
            innerSubstituted <- innerTail.makeSingleSubstitution(termVariable, innerVariable, DistinctVariables.empty)
            result <- innerSubstituted.makeSingleSubstitution(firstTerm, firstVariable, DistinctVariables.empty)
            distinctVariables <- DistinctVariables.attempt(
              termVariable -> innerTail,
              termVariable -> firstTerm,
              termVariable -> firstVariable,
              firstVariable -> thisTerm,
              firstVariable -> otherTerm)
          } yield (result.asInstanceOf[T], distinctVariables ++ validationDistinctVariables)
        case _ =>
          None
      }
    }
  }

  override def findSubstitution(
    target: Component,
    termVariableToBeReplaced: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (this == target) {
      (Seq((termVariableToBeReplaced, DistinctVariables.empty)), DistinctVariables.attempt(termVariableToBeReplaced -> this))
    } else target match {
      case SubstitutedVariable(term, `termVariableToBeReplaced`, otherTail) if otherTail == this =>
        (Seq((term, DistinctVariables.empty)), None)
      case x if x == tail && firstTerm == termVariableToBeReplaced =>
        // [x/y][y/x]φ is φ if y is distinct from φ
        (Seq((firstVariable, DistinctVariables(termVariableToBeReplaced -> variable))), None)
      case SubstitutedVariable(otherTerm, `firstVariable`, otherTail) if tail == otherTail && firstTerm == termVariableToBeReplaced =>
        // [z/y][y/x]φ is [z/x]φ if y is distinct from φ
        (Seq((otherTerm, DistinctVariables(termVariableToBeReplaced -> variable))), None)
      case _ =>
        (Nil, None)
    }
  }
  override def findDoubleSubstitution(
    target: Component,
    firstTermVariable: TermVariable,
    firstTerm: Term,
    secondTermVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    makeSingleSubstitution(firstTerm, firstTermVariable, DistinctVariables.empty) match {
      case Some(substituted) =>
        substituted.findSubstitution(target, secondTermVariable)
      case None =>
        (Nil, None)
    }
  }

  override def replacePlaceholder(other: Component): Option[T] = Some(this.asInstanceOf[T])

  protected override def condenseOneWay(
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    super.condenseOneWay(other, thisSubstitutions, otherSubstitutions) orElse (for {
      updatedTail <- tail.applySubstitutions(thisSubstitutions.knownSubstitutions)
      updatedTermToReplaceWith <- firstTerm.applySubstitutions(thisSubstitutions.knownSubstitutions)
      updatedTermToBeReplaced <- firstVariable.applySubstitutions(thisSubstitutions.knownSubstitutions).flatMap(Term.optionAsVariable)
      result <- updatedTail.condenseWithSubstitution(
        updatedTermToReplaceWith,
        updatedTermToBeReplaced,
        other,
        thisSubstitutions,
        otherSubstitutions)
    } yield result)
  }

  override def toString: String = "[" + firstTerm.safeToString + "/" + firstVariable.toString + "]" + tail.toString
  override def html: String = "[" + firstTerm.safeHtml + "/" + firstVariable.html + "]" + tail.html
  override def serialized: String = Seq(
    "sub",
    firstTerm.serialized,
    firstVariable.serialized,
    tail.serialized
  ).mkString(" ")
}

object SubstitutedVariable {
  def unapply(
    substitutedVariable: SubstitutedVariable[Component, _ <: Variable]
  ): Option[(Term, TermVariable, Component)] = {
    Some((
      substitutedVariable.firstTerm,
      substitutedVariable.firstVariable,
      substitutedVariable.tail))
  }
}
