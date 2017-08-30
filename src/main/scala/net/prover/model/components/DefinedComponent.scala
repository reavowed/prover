package net.prover.model.components

import net.prover.model._

import scala.collection.immutable.Nil
import scala.util.Try

trait DefinedComponent[T <: Component] extends Component {
  def subcomponents: Seq[Component]
  def format: Format
  def symbol: String

  def getMatch(other: Component): Option[Seq[Component]]
  def update(newSubcomponents: Seq[Component]): T

  override def variables: Seq[Variable] = subcomponents.flatMap(_.variables).distinct

  override def calculateSubstitutions(
    other: Component,
    substitutions: Substitutions
  ): Seq[Substitutions] = {
    getMatch(other).map { otherSubcomponents =>
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
    } yield {
      update(updatedSubcomponents)
    }
  }

  override def replacePlaceholder(other: Component) = {
    for {
      updatedSubcomponents <- subcomponents.map(_.replacePlaceholder(other)).traverseOption
      updated <- Try(update(updatedSubcomponents)).toOption
    } yield updated
  }

  override def condense(
    other: Component,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ): Option[(Substitutions, Substitutions)] = {
    super.condense(other, thisSubstitutions, otherSubstitutions) orElse
      getMatch(other).flatMap { otherSubcomponents =>
        subcomponents.zip(otherSubcomponents)
          .foldInAnyOrder((thisSubstitutions, otherSubstitutions)) {
            case ((thisSubstitutionsSoFar, otherSubstitutionsSoFar), (thisSubcomponent, otherSubcomponent)) =>
              thisSubcomponent.condense(otherSubcomponent, thisSubstitutionsSoFar, otherSubstitutionsSoFar)
          }
      }
  }

  override def findSubcomponent(other: Component): Option[Seq[Int]] = {
    super.findSubcomponent(other) orElse
      subcomponents.zipWithIndex.mapFind { case (subcomponent, index) =>
        subcomponent.findSubcomponent(other).map(index +: _)
      }
  }

  override def toString: String = {
    format(subcomponents.map(_.safeToString))
  }
  override def safeToString: String = {
    format.safe(subcomponents.map(_.safeToString))
  }
  override def serialized: String = (symbol +: subcomponents.map(_.serialized)).mkString(" ")
}
