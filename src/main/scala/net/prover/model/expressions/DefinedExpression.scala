package net.prover.model.expressions

import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.substitutionFinding.transformers.ParameterRemover

import scala.collection.immutable.Nil

trait DefinedExpression[ExpressionType <: Expression] extends Expression with TypedExpression[ExpressionType] {
  def components: Seq[Expression]
  def boundVariableNames: Seq[String]
  def definition: CompoundExpressionDefinition

  def getMatch(other: Expression): Option[Seq[Expression]]
  def updateComponents(newComponents: Seq[Expression]): ExpressionType
  def updateBoundVariableNames(newBoundVariableNames: Seq[String]): ExpressionType

  override def usedVariables: UsedVariables = components.usedVariables
  override def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, ExpressionType, Int, Seq[Int])] = {
    @scala.annotation.tailrec
    def helper(previous: Seq[Expression], next: Seq[Expression], acc: Seq[(Term, ExpressionType, Int, Seq[Int])]): Seq[(Term, ExpressionType, Int, Seq[Int])] = {
      next match {
        case current +: more =>
          helper(
            previous :+ current,
            more,
            acc ++ current.getTerms(definition.increaseDepth(internalDepth), externalDepth).map { case (term, function, depth, path) =>
              (term, updateComponents((previous :+ function) ++ more), depth, previous.length +: path)
            })
        case _ =>
          acc
      }
    }
    helper(Nil, components, Nil).mapCollect(_.optionMap1(t => if (boundVariableNames.isEmpty) Some(t) else ParameterRemover.removeParameters(t, 1, 0)))
  }

  override def getPredicateForTerm(term: Term, depth: Int): ExpressionType = {
    updateComponents(components.map(_.getPredicateForTerm(term, definition.increaseDepth(depth))))
  }

  override def renameBoundVariable(newName: String, index: Int, path: Seq[Int]): Option[ExpressionType] = {
    path match {
      case Nil =>
        if (boundVariableNames.lift(index).nonEmpty)
          Some(updateBoundVariableNames(boundVariableNames.updated(index, newName)))
        else
          None
      case head +: tail =>
        components.lift(head).flatMap(_.renameBoundVariable(newName, index, tail)).map(e => updateComponents(components.updated(head, e)))
    }
  }

  override def findComponentPath(other: Expression): Option[Seq[Int]] = {
    super.findComponentPath(other) orElse
      components.zipWithIndex.mapFind { case (subcomponent, index) =>
        subcomponent.findComponentPath(other).map(index +: _)
      }
  }

  override def toString: String = {
    definition.format.formatText(boundVariableNames ++ components.map(_.safeToString), definition.symbol, parentRequiresBrackets = false)
  }
  override def safeToString: String = {
    definition.format.formatText(boundVariableNames ++ components.map(_.safeToString), definition.symbol, parentRequiresBrackets = true)
  }
  override def serialized: String = (Seq(definition.disambiguatedSymbol.serialized) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
  override def serializedForHash: String = (Seq(definition.disambiguatedSymbol.serialized) ++ components.map(_.serializedForHash)).mkString(" ")
}
