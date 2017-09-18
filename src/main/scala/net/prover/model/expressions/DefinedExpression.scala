package net.prover.model.expressions

import net.prover.model._

import scala.collection.immutable.Nil

trait DefinedExpression[T <: Expression] extends Expression {
  def components: Seq[Expression]
  def scopedBoundVariableNames: Seq[String]
  def format: Format
  def symbol: String

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newComponents: Seq[Expression]): T

  override def boundVariables = components.flatMap(_.boundVariables).toSet
  override def requiredSubstitutions = components.map(_.requiredSubstitutions).foldTogether

  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    getMatch(other).map { otherComponents =>
      components.zip(otherComponents)
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (component, otherComponent)) =>
          substitutionsSoFar.flatMap(component.calculateSubstitutions(otherComponent, _, boundVariableCount + scopedBoundVariableNames.length))
        }
    }
    .getOrElse(Nil)
  }

  override def applySubstitutions(substitutions: Substitutions): Option[T] = {
    for {
      updatedComponents <- components.map(_.applySubstitutions(substitutions)).traverseOption
    } yield {
      update(updatedComponents)
    }
  }

  override def replacePlaceholder(other: Expression) = {
    val updatedComponents = components.map(_.replacePlaceholder(other))
    update(updatedComponents)
  }

  override def findComponentPath(other: Expression): Option[Seq[Int]] = {
    super.findComponentPath(other) orElse
      components.zipWithIndex.mapFind { case (subcomponent, index) =>
        subcomponent.findComponentPath(other).map(index +: _)
      }
  }

  override def toString: String = {
    format(scopedBoundVariableNames ++ components.map(_.safeToString))
  }
  override def safeToString: String = {
    format.safe(scopedBoundVariableNames ++ components.map(_.safeToString))
  }
  override def serialized: String = (Seq(symbol) ++ scopedBoundVariableNames ++ components.map(_.serialized)).mkString(" ")
}
