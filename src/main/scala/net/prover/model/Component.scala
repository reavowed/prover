package net.prover.model

import scala.collection.immutable.Nil
import scala.util.Try

trait Component {
  def componentType: ComponentType

  /**
    * All the variables that appear syntactically in this component and must
    * be supplied in a general substitution.
    */
  def allVariables: Variables

  /**
    * The variables that definitely appear in this component and to which
    * distinct variable conditions must be carried over. Mostly the same as
    * `allVariables`, except that `x` is not present in [y/x]φ.
    */
  def presentVariables: Variables
  def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable])
  def boundVariables: Set[TermVariable] = boundAndFreeVariables._1
  def freeVariables: Set[TermVariable] = boundAndFreeVariables._2
  def implicitDistinctVariables: DistinctVariables
  def getPotentiallyIntersectingVariables(variable: Variable): Variables
  def calculateSubstitutions(other: Component, substitutions: PartialSubstitutions): Seq[PartialSubstitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Component]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Component]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Component, DistinctVariables)]
  def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables]
  def findSubstitution(other: Component, termVariable: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
  def replacePlaceholder(other: Component): Option[Component]
  def html: String
  def safeHtml: String = html
  def serialized: String
  override def toString: String = html
}

trait DefinedComponent[T <: Component] extends Component {
  def subcomponents: Seq[Component]
  def localBoundVariables: Set[TermVariable]

  def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])]
  def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): T

  override def allVariables: Variables = subcomponents.map(_.allVariables).foldLeft(Variables.empty)(_ ++ _)
  override def presentVariables: Variables = subcomponents.map(_.presentVariables).foldLeft(Variables.empty)(_ ++ _)
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
      subcomponentTermVariable <- subcomponents.map(_.presentVariables).foldTogether.termVariables -- localBoundVariables
    } yield (localBoundVariable, subcomponentTermVariable)
    val newImplicitDistinctVariables =  implicitPairs.foldLeft(DistinctVariables.empty) { case (dv, (bv, tv)) =>
      dv + (bv, tv)
    }
    subcomponentImplicitDistinctVariables ++ newImplicitDistinctVariables
  }
  def getPotentiallyIntersectingVariables(variable: Variable): Variables = {
    variable match {
      case termVariable: TermVariable if localBoundVariables.contains(termVariable) =>
        Variables.empty
      case _ =>
        subcomponents
          .map(_.getPotentiallyIntersectingVariables(variable))
          .foldLeft(Variables.empty)(_ ++ _) -- localBoundVariables
    }
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
    for {
      updatedSubcomponents <- subcomponents
        .map(_.makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables))
        .traverseOption
    } yield update(updatedSubcomponents, localBoundVariables)
  }
  override def validateSubstitution(
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
              subcomponent.validateSubstitution(termToReplaceWith, termToBeReplaced, otherSubcomponent, distinctVariables)
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
            thisTerm.presentVariables.termVariables.exists(localBoundVariables.contains) ||
            otherBoundVariables.contains(termVariable) ||
            otherTerm.presentVariables.termVariables.exists(otherBoundVariables.contains)
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
                variable <- thisTerm.presentVariables.termVariables -- localBoundVariables
              } yield boundVariable -> variable
              val otherDistinctPairs = for {
                boundVariable <- otherBoundVariables
                variable <- otherTerm.presentVariables.termVariables -- otherBoundVariables
              } yield boundVariable -> variable
              val additionalDistinctVariables = DistinctVariables((thisDistinctPairs ++ otherDistinctPairs).toSeq :_*)
              (update(resolvedSubcomponents, localBoundVariables), resolutionDistinctVariables ++ additionalDistinctVariables)
            }
        }
    }
  }
  def findSubstitution(other: Component, termVariable: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    getMatch(other)
      .map {
        case (otherSubcomponents, _) =>
          def combine(
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
          if (subcomponents.isEmpty) {
            (Nil, Some(DistinctVariables.empty))
          } else {
            subcomponents.zip(otherSubcomponents)
              .map {
                case (subcomponent, otherSubcomponent) =>
                  subcomponent.findSubstitution(otherSubcomponent, termVariable)
              }
              .reduce(combine)
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
  override def allVariables: Variables = Variables.empty
  override def presentVariables: Variables = Variables.empty
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set.empty)
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  override def getPotentiallyIntersectingVariables(variable: Variable): Variables = Variables.empty
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
  override def validateSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, target: Component, distinctVariables: DistinctVariables) = {
    throw new Exception("Cannot validate substitution for placeholder")
  }
  def findSubstitution(other: Component, termVariable: TermVariable) = {

    throw new Exception("Cannot find substitution for placeholder")
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
