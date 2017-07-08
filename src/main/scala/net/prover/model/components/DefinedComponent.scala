package net.prover.model.components

import net.prover.model._

import scala.collection.immutable.Nil
import scala.util.{Failure, Success, Try}

trait DefinedComponent[T <: Component] extends Component {
  def subcomponents: Seq[Component]
  def localBoundVariables: Set[TermVariable]

  def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])]
  def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): T

  override def allVariables: Set[Variable] = subcomponents.flatMap(_.allVariables).toSet
  override def presentVariables: Set[Variable] = subcomponents.flatMap(_.presentVariables).toSet
  override val boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = {
    DefinedComponent.tryBoundAndFree(subcomponents, localBoundVariables).get
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
  override def getInternalDifferences(other: Component): Set[(Component, Component)] = {
    getMatch(other)
      .map {
        case (otherSubcomponents, _) =>
          subcomponents.zip(otherSubcomponents)
            .map { case (a, b) => a.getDifferences(b) }
            .foldLeft(Set.empty[(Component, Component)])(_ ++ _)
      }
      .getOrElse(Set.empty)
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
      _ <- DefinedComponent.tryBoundAndFree(updatedSubcomponents, updatedBoundVariables).toOption
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

object DefinedComponent {
  def tryBoundAndFree(subcomponents: Seq[Component], localBoundVariables: Set[TermVariable]): Try[(Set[TermVariable], Set[TermVariable])] = {
    for {
      (mergedBound, mergedFree) <- subcomponents.foldLeft(Try((Set.empty[TermVariable], Set.empty[TermVariable]))) {
        case (Success((boundVariables, freeVariables)), subcomponent) =>
          val (subcomponentBound, subcomponentFree) = subcomponent.boundAndFreeVariables
          val newBound = boundVariables ++ subcomponentBound
          val newFree = freeVariables ++ subcomponentFree
          newBound.intersect(newFree).headOption match {
            case Some(v) =>
              Failure(new Exception(s"Variable $v appears both bound and free in $this"))
            case None =>
              Success((newBound, newFree))
          }
        case (failure, _) => failure
      }
      _ <- mergedBound.intersect(localBoundVariables).headOption match {
        case Some(v) => Failure(new Exception(s"Variable $v is bound twice in $this"))
        case None => Success(())
      }
    } yield {
      (mergedBound ++ localBoundVariables, mergedFree -- localBoundVariables)
    }
  }
}
