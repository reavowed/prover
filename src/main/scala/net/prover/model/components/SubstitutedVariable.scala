package net.prover.model.components

import net.prover.model.{DistinctVariables, PartialSubstitutions, Substitutions}

import scala.collection.immutable.Nil

trait SubstitutedVariable[+T <: Component, TVariable <: Variable] extends Component {
  val variable: TVariable
  val termToReplaceWith: Term
  val termToBeReplaced: TermVariable

  def update(updatedVariable: TVariable, updatedTermToReplaceWith: Term, updatedTermToBeReplaced: TermVariable): T

  override def allVariables: Set[Variable] = termToReplaceWith.allVariables + termToBeReplaced + variable
  override def presentVariables: Set[Variable] = termToReplaceWith.allVariables + variable
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = termToReplaceWith.boundAndFreeVariables
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = {
    if (termVariable == termToBeReplaced)
      termToReplaceWith.getPotentiallyIntersectingVariables(termVariable)
    else
      termToReplaceWith.getPotentiallyIntersectingVariables(termVariable) + variable
  }
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = other match {
    case SubstitutedVariable(otherVariable, otherTermToReplaceWith, otherTermToBeReplaced) =>
      for {
        s1 <- variable.calculateSubstitutions(otherVariable, substitutions)
        s2 <- termToReplaceWith.calculateSubstitutions(otherTermToReplaceWith, s1)
        s3 <- termToBeReplaced.calculateSubstitutions(otherTermToBeReplaced, s2)
      } yield s3
    case statement: Statement =>
      substitutions.tryAdd(this, statement)
    case _ =>
      Nil
  }
  override def applySubstitutions(substitutions: Substitutions): Option[T] = {
    for {
      updatedVariable <- variable.applySubstitutions(substitutions)
      updatedTermToReplaceWith <- termToReplaceWith.applySubstitutions(substitutions)
      updatedTermToBeReplaced <- termToBeReplaced.applySubstitutions(substitutions).flatMap(Term.optionAsVariable)
      updatedStatement <- updatedVariable.makeSingleSubstitution(
        updatedTermToReplaceWith,
        updatedTermToBeReplaced,
        substitutions.distinctVariables)
    } yield updatedStatement.asInstanceOf[T]
  }
  override def makeSingleSubstitution(
    newTermToReplaceWith: Term,
    newTermToBeReplaced: TermVariable,
    distinctVariables: DistinctVariables
  ): Option[T] = {
    if (newTermToReplaceWith == newTermToBeReplaced)
      Some(this.asInstanceOf[T])
    else if (newTermToBeReplaced == termToBeReplaced && distinctVariables.areDistinct(termToBeReplaced, termToReplaceWith))
      Some(this.asInstanceOf[T])
    else if (newTermToBeReplaced == termToReplaceWith && distinctVariables.areDistinct(newTermToBeReplaced, variable))
      if (newTermToReplaceWith == termToBeReplaced)
        Some(variable.asInstanceOf[T])
      else
        Some(update(variable, newTermToReplaceWith, termToBeReplaced))
    else
      termToReplaceWith match {
        case termVariableToReplaceWith: TermVariable
          if distinctVariables.areDistinct(newTermToBeReplaced, variable) && distinctVariables.areDistinct(newTermToBeReplaced, termVariableToReplaceWith)
        =>
          Some(this.asInstanceOf[T])
        case _ =>
          None
      }
  }
  override def validateSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    if (makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables).contains(target)) {
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
  ): Option[(T, DistinctVariables)] = {
    if (other == this) {
      Some((this.asInstanceOf[T], DistinctVariables(termVariable -> getPotentiallyIntersectingVariables(termVariable))))
    } else {
      other match {
        case SubstitutedVariable(`variable`, `otherTerm`, `termToBeReplaced`) if termToReplaceWith == thisTerm =>
          Some((update(variable, termVariable, termToBeReplaced), DistinctVariables(termVariable -> variable)))
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
      (Seq((termVariableToBeReplaced, DistinctVariables.empty)), DistinctVariables.attempt(termVariableToBeReplaced, this))
    } else if (termToReplaceWith == termVariableToBeReplaced) {
      target match {
        // [x/y][y/x]φ is φ if y is distinct from φ
        case `variable` =>
          (Seq((termToBeReplaced, DistinctVariables(termVariableToBeReplaced -> variable))), None)
        // [z/y][y/x]φ is [z/x]φ if y is distinct from φ
        case SubstitutedStatementVariable(`variable`, otherTermToReplaceWith: TermVariable, `termToBeReplaced`) =>
          (Seq((otherTermToReplaceWith, DistinctVariables(termVariableToBeReplaced -> variable))), None)
        case _ =>
          (Nil, None)
      }
    } else {
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
      updatedVariable <- variable.applySubstitutions(thisSubstitutions.knownSubstitutions)
      updatedTermToReplaceWith <- termToReplaceWith.applySubstitutions(thisSubstitutions.knownSubstitutions)
      updatedTermToBeReplaced <- termToBeReplaced.applySubstitutions(thisSubstitutions.knownSubstitutions).flatMap(Term.optionAsVariable)
      result <- updatedVariable.condenseWithSubstitution(
        updatedTermToReplaceWith,
        updatedTermToBeReplaced,
        other,
        thisSubstitutions,
        otherSubstitutions)
    } yield result)
  }

  override def html: String = "[" + termToReplaceWith.safeHtml + "/" + termToBeReplaced.html + "]" + variable.html
  override def serialized: String = Seq(
    "sub",
    termToReplaceWith.serialized,
    termToBeReplaced.serialized,
    variable.serialized
  ).mkString(" ")
}

object SubstitutedVariable {
  def unapply(component: Component): Option[(Variable, Term, TermVariable)] = {
    component match {
      case substitutedVariable: SubstitutedVariable[Component, _] =>
        Some((substitutedVariable.variable, substitutedVariable.termToReplaceWith, substitutedVariable.termToBeReplaced))
      case _ =>
        None
    }
  }
}
