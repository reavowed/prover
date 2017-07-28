package net.prover.model.components

import net.prover.model._

import scala.collection.immutable.Nil
import scala.util.{Failure, Success, Try}

trait DefinedComponent[T <: Component] extends Component {
  def subcomponents: Seq[Component]
  def localBoundVariables: Set[TermVariable]
  def format: Format
  def symbol: String

  def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])]
  def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): T

  override def variables: Seq[Variable] = subcomponents.flatMap(_.variables).distinct
  override val boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = {
    DefinedComponent.tryBoundAndFree(subcomponents, localBoundVariables).get
  }

  override def calculateSubstitutions(
    other: Component,
    substitutions: Substitutions
  ): Seq[Substitutions] = {
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

  override def replacePlaceholder(other: Component) = {
    for {
      updatedSubcomponents <- subcomponents.map(_.replacePlaceholder(other)).traverseOption
      updated <- Try(update(updatedSubcomponents, localBoundVariables)).toOption
    } yield updated
  }

  override def condense(
    other: Component,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ): Option[(Substitutions, Substitutions)] = {
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

  override def toString: String = {
    format(subcomponents.map(_.safeToString))
  }
  override def safeToString: String = {
    format.safe(subcomponents.map(_.safeToString))
  }
  override def html: String = {
    Html.format(format(subcomponents.map(_.safeHtml)))
  }
  override def safeHtml: String = {
    Html.format(format.safe(subcomponents.map(_.safeHtml)))
  }
  override def serialized: String = (symbol +: subcomponents.map(_.serialized)).mkString(" ")
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
