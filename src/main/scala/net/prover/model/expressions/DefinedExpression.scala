package net.prover.model.expressions

import net.prover.model._

import scala.collection.immutable.Nil

trait DefinedExpression[Applicable <: Expression, NonApplicable <: Applicable] extends Expression {
  def components: Seq[Expression]
  def scopedBoundVariableNames: Seq[String]
  def format: Format
  def symbol: String
  def defaultVariables: Seq[Variable]

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newComponents: Seq[Expression]): NonApplicable
  def updateApplicable(newComponents: Seq[Expression], depth: Int): Applicable

  override def requiredSubstitutions = components.requiredSubstitutions

  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    getMatch(other)
      .map(components.calculateSubstitutions(_, substitutions))
      .getOrElse(Nil)
  }

  override def applySubstitutions(substitutions: Substitutions): Option[Applicable] = {
    components.applySubstitutions(substitutions).map { newSubcomponents =>
      if (newSubcomponents.isEmpty)
        update(newSubcomponents)
      else
        defaultVariables.head.depthDifference(newSubcomponents.head) match {
          case Some(0) =>
            update(newSubcomponents)
          case Some(n) =>
            updateApplicable(newSubcomponents, n)
          case None =>
            throw new Exception("Invalid subcomponent")
        }
    }
  }

  override def replacePlaceholder(other: Expression): NonApplicable = {
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
