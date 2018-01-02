package net.prover.model.expressions

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

import scala.collection.immutable.Nil

trait DefinedExpression[ExpressionType <: Expression] extends Expression {
  def components: Seq[Expression]
  def scopedBoundVariableNames: Seq[String]
  def definition: ExpressionDefinition

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newComponents: Seq[Expression], newDepth: Int): ExpressionType

  override def reduceDepth(difference: Int, insertionPoint: Int): Option[ExpressionType] = {
    if (depth >= difference)
      components.map(_.reduceDepth(difference, insertionPoint)).traverseOption.map(update(_, depth - difference))
    else
      None
  }
  override def specify(targetArguments: ArgumentList): ExpressionType = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    update(components.map(_.specify(targetArguments)), depth - 1)
  }
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions
  ) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    components
      .map(_.specifyWithSubstitutions(targetArguments, substitutions)).traverseOption
      .map(update(_, depth + targetArguments.depth - 1))
  }
  override def increaseDepth(additionalDepth: Int, insertionPoint: Int): ExpressionType = {
    update(components.map(_.increaseDepth(additionalDepth, insertionPoint)), depth + additionalDepth)
  }

  override def requiredSubstitutions = components.requiredSubstitutions
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ) = {
    getMatch(other)
      .map(components.calculateSubstitutions(_, substitutions, applicativeHints, structuralHints))
      .getOrElse(Nil)
  }
  override def applySubstitutions(substitutions: Substitutions): Option[ExpressionType] = {
    components.applySubstitutions(substitutions).map(update(_, depth + substitutions.depth))
  }
  override def calculateApplicatives(baseArguments: ArgumentList, substitutions: Substitutions): Seq[(ExpressionType, Substitutions)] = {
    components.calculateApplicatives(baseArguments, substitutions)
      .map(_.mapLeft(update(_, depth - baseArguments.depth + 1)))
  }

  override def condense(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ) = {
    super.condense(other, thisSubstitutions, otherSubstitutions, applicativeHints, structuralHints) ++
      getMatch(other).toSeq.flatMap { otherComponents =>
        components.zip(otherComponents)
          .flatMapFoldProduct((thisSubstitutions, otherSubstitutions, Seq.empty[(Substitutions, ArgumentList)], Seq.empty[Substitutions]))
          { case ((thisSubstitutionsSoFar, otherSubstitutionsSoFar, applicativeHintsSoFar, structuralHintsSoFar), (component, otherComponent)) =>
            component.condense(otherComponent, thisSubstitutionsSoFar, otherSubstitutionsSoFar, applicativeHints, structuralHints)
              .map(t => (t._1, t._2, applicativeHintsSoFar ++ t._3, structuralHintsSoFar ++ t._4))
          }
      }
  }

  def matchesStructure(other: Expression): Boolean = {
    getMatch(other).exists { otherComponents =>
      components.zipStrict(otherComponents).exists { componentsAndOtherComponents =>
        componentsAndOtherComponents.forall { case (component, otherComponent) =>
          component.matchesStructure(otherComponent)
        }
      }
    }
  }
  override def findComponentPath(other: Expression): Option[Seq[Int]] = {
    super.findComponentPath(other) orElse
      components.zipWithIndex.mapFind { case (subcomponent, index) =>
        subcomponent.findComponentPath(other).map(index +: _)
      }
  }

  override def toString: String = {
    definition.format(scopedBoundVariableNames ++ components.map(_.safeToString))
  }
  override def safeToString: String = {
    definition.format.safe(scopedBoundVariableNames ++ components.map(_.safeToString))
  }
  override def serialized: String = (Seq(definition.symbol) ++ scopedBoundVariableNames ++ components.map(_.serialized)).mkString(" ")
}

object DefinedExpression {
  def unapply(expression: DefinedExpression[_]): Option[(ExpressionDefinition, Seq[Expression])] = expression match {
    case definedExpression: DefinedExpression[_] =>
      Some((definedExpression.definition, definedExpression.components))
    case _ =>
      None
  }
}
