package net.prover.model

import scala.collection.immutable.Nil
import scala.util.Try

trait Component {
  def componentType: ComponentType

  /**
    * All the variables that appear syntactically in this component and must
    * be supplied in a general substitution.
    */
  def allVariables: Set[Variable]

  /**
    * The variables that definitely appear in this component and to which
    * distinct variable conditions must be carried over. Mostly the same as
    * `allVariables`, except that `x` is not present in [y/x]φ.
    */
  def presentVariables: Set[Variable]
  def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable])
  def boundVariables: Set[TermVariable] = boundAndFreeVariables._1
  def freeVariables: Set[TermVariable] = boundAndFreeVariables._2
  def implicitDistinctVariables: DistinctVariables
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable]
  def calculateSubstitutions(other: Component, substitutions: PartialSubstitutions): Seq[PartialSubstitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Component]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Component]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Component, DistinctVariables)]
  def validateSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables]
  def findSubstitution(target: Component, termVariableToBeReplaced: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
  def findDoubleSubstitution(
    target: Component,
    firstTermVariable: TermVariable,
    firstTerm: Term,
    secondTermVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
  def replacePlaceholder(other: Component): Option[Component]
  def condense(
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    condenseOneWay(other, thisSubstitutions, otherSubstitutions) orElse
      other.condenseOneWay(this, otherSubstitutions, thisSubstitutions).map(_.reverse)
  }
  protected def condenseOneWay(
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    for {
      thisSubstituted <- applySubstitutions(thisSubstitutions.knownSubstitutions)
      updatedOtherSubstitutions <- other.calculateSubstitutions(thisSubstituted, otherSubstitutions).headOption
    } yield (thisSubstitutions, updatedOtherSubstitutions)
  }
  def condenseWithSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    other match {
      case statementVariable: StatementVariable =>
        otherSubstitutions.unknown.toSeq.mapCollect {
          case (SubstitutedStatementVariable(`statementVariable`, otherTermToReplaceWith, otherTermToBeReplaced), otherTarget) =>
            otherSubstitutions.known.get(otherTermToBeReplaced).flatMap(Term.optionAsVariable).flatMap { substitutedTermToBeReplaced =>
              for {
                (substitutedOtherTermToReplaceWith, newDistinctVariables) <-
                  findDoubleSubstitution(otherTarget, termToBeReplaced, termToReplaceWith, substitutedTermToBeReplaced)
                    ._1.headOption
                updatedOtherSubstitutions <- otherTermToReplaceWith
                  .calculateSubstitutions(
                    substitutedOtherTermToReplaceWith,
                    otherSubstitutions.withDistinctVariables(newDistinctVariables))
                  .headOption
              } yield (thisSubstitutions.withDistinctVariables(newDistinctVariables), updatedOtherSubstitutions)
            }
        }.headOption
      case _ =>
        None
    }
  }
  def html: String
  def safeHtml: String = html
  def serialized: String
  override def toString: String = html
}

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

trait DefinedComponent[T <: Component] extends Component {
  def subcomponents: Seq[Component]
  def localBoundVariables: Set[TermVariable]

  def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])]
  def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): T

  override def allVariables: Set[Variable] = subcomponents.flatMap(_.allVariables).toSet
  override def presentVariables: Set[Variable] = subcomponents.flatMap(_.presentVariables).toSet
  override val boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = {
    val (mergedBound, mergedFree) = subcomponents.foldLeft((Set.empty[TermVariable], Set.empty[TermVariable])) {
      case ((boundVariables, freeVariables), subcomponent) =>
        val (subcomponentBound, subcomponentFree) = subcomponent.boundAndFreeVariables
        val newBound = boundVariables ++ subcomponentBound
        val newFree = freeVariables ++ subcomponentFree
        newBound.intersect(newFree).headOption.foreach { v =>
          throw new Exception(s"Variable $v appears both bound and free in $this")
        }
        (newBound, newFree)
    }
    mergedBound.intersect(localBoundVariables).headOption.foreach { v => throw new Exception(s"Variable $v is bound twice in $this")}
    (mergedBound ++ localBoundVariables, mergedFree -- localBoundVariables)
  }
  override def implicitDistinctVariables: DistinctVariables = {
    val subcomponentImplicitDistinctVariables = subcomponents.map(_.implicitDistinctVariables).foldTogether
    val implicitPairs = for {
      localBoundVariable <- localBoundVariables
      subcomponentTermVariable <- subcomponents.flatMap(_.presentVariables).toSet.ofType[TermVariable]-- localBoundVariables
    } yield (localBoundVariable, subcomponentTermVariable)
    val newImplicitDistinctVariables =  implicitPairs.foldLeft(DistinctVariables.empty) { case (dv, (bv, tv)) =>
      dv + (bv, tv)
    }
    subcomponentImplicitDistinctVariables ++ newImplicitDistinctVariables
  }
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = {
    if (localBoundVariables.contains(termVariable))
      Set.empty
    else
      subcomponents.flatMap(_.getPotentiallyIntersectingVariables(termVariable)).toSet -- localBoundVariables
  }

  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = {
    getMatch(other).map {
      case (otherSubcomponents, _) =>
        subcomponents.zip(otherSubcomponents)
          .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (component, otherComponent)) =>
            substitutionsSoFar.flatMap(component.calculateSubstitutions(otherComponent, _))
          }
    }
    .getOrElse(Nil)
  }

  override def applySubstitutions(substitutions: Substitutions): Option[T] = {
    for {
      updatedSubcomponents <- subcomponents.map(_.applySubstitutions(substitutions)).traverseOption
      updatedBoundVariables <- localBoundVariables
        .map(_.applySubstitutions(substitutions).flatMap(Term.optionAsVariable))
        .traverseOption
    } yield {
      update(updatedSubcomponents, updatedBoundVariables)
    }
  }

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[T] = {
    if (termToReplaceWith == termToBeReplaced)
      Some(this.asInstanceOf[T])
    else if (localBoundVariables.contains(termToBeReplaced) || localBoundVariables.exists(v => !distinctVariables.areDistinct(v, termToBeReplaced)))
      None
    else
      for {
        updatedSubcomponents <- subcomponents
          .map(_.makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables))
          .traverseOption
      } yield update(updatedSubcomponents, localBoundVariables)
  }
  override def validateSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    getMatch(target)
      .flatMap {
        case (otherSubcomponents, _) =>
          subcomponents.zip(otherSubcomponents)
            .map { case (subcomponent, otherSubcomponent) =>
              subcomponent.validateSingleSubstitution(termToReplaceWith, termToBeReplaced, otherSubcomponent, distinctVariables)
            }
            .traverseOption
            .map(_.foldTogether)
      }
  }

  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(T, DistinctVariables)] = {
    getMatch(other).flatMap {
      case (otherSubcomponents, otherBoundVariables) =>
        if (
          localBoundVariables.contains(termVariable) ||
            thisTerm.presentVariables.ofType[TermVariable].exists(localBoundVariables.contains) ||
            otherBoundVariables.contains(termVariable) ||
            otherTerm.presentVariables.ofType[TermVariable].exists(otherBoundVariables.contains)
        )
          None
        else {
          subcomponents.zip(otherSubcomponents)
            .map { case (subcomponent, otherSubcomponent) =>
              subcomponent.resolveSingleSubstitution(otherSubcomponent, termVariable, thisTerm, otherTerm)
            }
            .traverseOption
            .map(_.split)
            .map(_.mapRight(_.foldTogether))
            .map { case (resolvedSubcomponents, resolutionDistinctVariables) =>
              val thisDistinctPairs = for {
                boundVariable <- localBoundVariables
                variable <- thisTerm.presentVariables.ofType[TermVariable] -- localBoundVariables
              } yield boundVariable -> variable
              val otherDistinctPairs = for {
                boundVariable <- otherBoundVariables
                variable <- otherTerm.presentVariables.ofType[TermVariable] -- otherBoundVariables
              } yield boundVariable -> variable
              val additionalDistinctVariables = DistinctVariables((thisDistinctPairs ++ otherDistinctPairs).toSeq :_*)
              (update(resolvedSubcomponents, localBoundVariables), resolutionDistinctVariables ++ additionalDistinctVariables)
            }
        }
    }
  }
  private def combineFoundSubstitutions(
    x: (Seq[(Term, DistinctVariables)], Option[DistinctVariables]),
    y: (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    val terms = (x._1.map(_._1) ++ y._1.map(_._1)).distinct
    val termSubstitutions = terms.map { term =>
      for {
        xSide <- x._1.find(_._1 == term).map(_._2).orElse(x._2)
        ySide <- y._1.find(_._1 == term).map(_._2).orElse(y._2)
      } yield (term, xSide ++ ySide)
    } collect {
      case Some(z) => z
    }
    val avoidingSubstitutions = for {
      xSide <- x._2
      ySide <- y._2
    } yield xSide ++ ySide
    (termSubstitutions, avoidingSubstitutions)
  }
  override def findSubstitution(
    target: Component,
    termVariableToBeReplaced: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    getMatch(target)
      .map {
        case (targetSubcomponents, _) =>
          if (subcomponents.isEmpty) {
            (Nil, Some(DistinctVariables.empty))
          } else {
            val x = subcomponents.zip(targetSubcomponents)
              .map {
                case (subcomponent, targetSubcomponent) =>
                  subcomponent.findSubstitution(targetSubcomponent, termVariableToBeReplaced)
              }
              x.reduce(combineFoundSubstitutions)
          }
      }
      .getOrElse((Nil, None))
  }
  override def findDoubleSubstitution(
    target: Component,
    firstTermVariable: TermVariable,
    firstTerm: Term,
    secondTermVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    getMatch(target)
      .map {
        case (targetSubcomponents, _) =>
          if (subcomponents.isEmpty) {
            (Nil, Some(DistinctVariables.empty))
          } else {
            val x = subcomponents.zip(targetSubcomponents)
              .map {
                case (subcomponent, targetSubcomponent) =>
                  subcomponent.findDoubleSubstitution(targetSubcomponent, firstTermVariable, firstTerm, secondTermVariable)
              }
            val y = x.reduce(combineFoundSubstitutions)
            y
          }
      }
      .getOrElse((Nil, None))

  }

  override def replacePlaceholder(other: Component) = {
    for {
      updatedSubcomponents <- subcomponents.map(_.replacePlaceholder(other)).traverseOption
      updated <- Try(update(updatedSubcomponents, localBoundVariables)).toOption
    } yield updated
  }

  override def condense(
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    super.condense(other, thisSubstitutions, otherSubstitutions) orElse
      getMatch(other).flatMap {
        case (otherSubcomponents, _) =>
          subcomponents.zip(otherSubcomponents)
            .foldInAnyOrder((thisSubstitutions, otherSubstitutions)) {
              case ((thisSubstitutionsSoFar, otherSubstitutionsSoFar), (thisSubcomponent, otherSubcomponent)) =>
                thisSubcomponent.condense(otherSubcomponent, thisSubstitutionsSoFar, otherSubstitutionsSoFar)
            }
      }
  }
  override def condenseWithSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    other: Component,
    thisSubstitutions: PartialSubstitutions,
    otherSubstitutions: PartialSubstitutions
  ): Option[(PartialSubstitutions, PartialSubstitutions)] = {
    super.condenseWithSubstitution(termToReplaceWith, termToBeReplaced, other, thisSubstitutions, otherSubstitutions) orElse
      getMatch(other).flatMap {
        case (otherSubcomponents, _) =>
          subcomponents.zip(otherSubcomponents)
            .foldInAnyOrder((thisSubstitutions, otherSubstitutions)) {
              case ((thisSubstitutionsSoFar, otherSubstitutionsSoFar), (thisSubcomponent, otherSubcomponent)) =>
                thisSubcomponent.condenseWithSubstitution(
                  termToReplaceWith,
                  termToBeReplaced,
                  otherSubcomponent,
                  thisSubstitutionsSoFar,
                  otherSubstitutionsSoFar)
            }
      }
  }
}

trait Variable extends Component {
  def text: String
}

object Variable {
  def parser(implicit context: Context): Parser[Variable] = {
    for {
      variableName <- Parser.singleWord
    } yield {
      context.statementVariableNames.find(_ == variableName).map(StatementVariable)
        .orElse(Term.findVariable(variableName))
        .getOrElse(throw new Exception(s"Unrecognised variable name '$variableName'"))
    }
  }
}

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

trait ComponentType {
  def parser(implicit context: Context): Parser[Component]
}

object ComponentType {
  private val componentTypesByName = Map(
    "term" -> Term,
    "statement" -> Statement)

  def parser: Parser[ComponentType] = {
    for {
      name <- Parser.singleWord
    } yield {
      componentTypesByName.getOrElse(
        name,
        throw new Exception(s"Unrecognised statement type $name"))
    }
  }

  def listParser: Parser[Seq[ComponentType]] = parser.listInParens(None)

  implicit class ComponentTypeSeqOps(componentTypes: Seq[ComponentType]) {
    def componentsParser(implicit context: Context) = {
      componentTypes.map(_.parser).traverseParser
    }
  }
}
