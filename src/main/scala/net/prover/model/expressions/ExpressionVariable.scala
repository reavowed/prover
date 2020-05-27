package net.prover.model.expressions

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition

import scala.reflect.ClassTag

abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression with TypedExpression[ExpressionType] with Substitutions.Lenses[ExpressionType] { this: ExpressionType =>
  def name: String
  def arguments: Seq[Term]
  val arity: Int = arguments.length

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newArguments: Seq[Term]): ExpressionType

  override def structuralComplexity: Int = 0
  override def definitionalComplexity: Int = 0

  override def definitionUsages: DefinitionUsages = arguments.map(_.definitionUsages).foldTogether
  override def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType = {
    update(arguments.map(_.insertExternalParameters(numberOfParametersToInsert, internalDepth)))
  }
  override def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType] = {
    arguments.map(_.removeExternalParameters(numberOfParametersToRemove, internalDepth)).traverseOption.map(update)
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): ExpressionType = {
    update(arguments.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }

  override def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, ExpressionType, Int, Seq[Int])] = {
    def helper(previous: Seq[Term], next: Seq[Term], acc: Seq[(Term, ExpressionType, Int, Seq[Int])]): Seq[(Term, ExpressionType, Int, Seq[Int])] = {
      next match {
        case current +: more =>
          helper(
            previous :+ current,
            more,
            acc ++ current.getTerms(internalDepth, externalDepth)
              .map { case (term, function, depth, path) =>
                (term, update((previous :+ function) ++ more), depth, previous.length +: path)
              })
        case _ =>
          acc
      }
    }
    helper(Nil, arguments, Nil)
  }
  override def getPredicateForTerm(term: Term, depth: Int): ExpressionType = {
    update(arguments.map(_.getPredicateForTerm(term, depth)))
  }

  def specify(
    targetArguments: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    arguments.map(_.specify(targetArguments, internalDepth, externalDepth)).traverseOption.map(update)
  }
  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    arguments.map(_.specifyWithSubstitutions(targetArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)).traverseOption.map(update)
  }

  override def requiredSubstitutions: Substitutions.Required = {
    arguments.requiredSubstitutions ++ requiredSubstitutionsLens.set(Seq((name, arity)))(Substitutions.Required.empty)
  }
  override def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Substitutions.Possible] = {
    if (other.isRuntimeInstance[ExpressionType]) {
      substitutionsLens.get(substitutions.stripApplications()).get(name) match {
        case Some((`arity`, applicative)) =>
          applicative.calculateArguments(other, Map.empty, internalDepth, 0, externalDepth).flatMap { otherArguments =>
            (0 until arity).foldLeft(Option(substitutions)) { case (substitutionOptions, index) =>
              substitutionOptions.flatMap { substitutionsSoFar =>
                otherArguments.get(index).map { otherArgument =>
                  arguments(index).calculateSubstitutions(otherArgument, substitutionsSoFar, internalDepth, externalDepth)
                }.getOrElse(Some(substitutionsSoFar))
              }
            }
          }
        case Some((otherArity, _)) =>
          None
        case None =>
          if (arguments.isEmpty) {
            for {
              reducedOther <- other.removeExternalParameters(internalDepth)
              result <- substitutions.update(name, arity, reducedOther.asInstanceOf[ExpressionType], possibleSubstitutionsLens)
            } yield result
          } else {
            substitutions
              .updateAdd(
                name,
                arity,
                (arguments, other.asInstanceOf[ExpressionType], internalDepth),
                possibleSubstitutionsApplicationsLens)
              .flatMap(_.clearApplicationsWherePossible(externalDepth))
          }
      }
    } else None
  }
  override def applySubstitutions(substitutions: Substitutions, internalDepth: Int, externalDepth: Int): Option[ExpressionType] = {
    for {
      (arity, predicate) <- substitutionsLens.get(substitutions).get(name)
      if arity == this.arity
      result <- predicate.specifyWithSubstitutions(arguments, substitutions, 0, internalDepth, externalDepth)
    } yield result.asInstanceOf[ExpressionType]
  }

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(ExpressionType, Substitutions.Possible)] = {
    arguments.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
      .map(_.mapLeft(newArguments => update(newArguments.map(_.asInstanceOf[Term]))))
  }
  override def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term],
    previousInternalDepth: Int,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Map[Int, Term]] = {
    getMatch(target).flatMap(targetComponents => arguments.calculateArguments(targetComponents, argumentsSoFar, previousInternalDepth, internalDepth, externalDepth))
  }

  override def toString: String = name + (if (arguments.nonEmpty) "(" + arguments.map(_.toString).mkString(", ") + ")" else "")
  override def serialized: String = (if (arguments.nonEmpty) "with (" + arguments.map(_.serialized).mkString(" ") + ") " else "") + name
  override def toStringForHash(variableDefinitions: VariableDefinitions): String = {
    (if (arguments.nonEmpty) "with (" + arguments.map(_.toStringForHash(variableDefinitions)).mkString(" ") + ") " else "") + getSymbolForHash(variableDefinitions)
  }
  def getSymbolForHash(variableDefinitions: VariableDefinitions): String
}

object ExpressionVariable {
  def unapply(expression: Expression): Option[(String, Seq[Term])] = expression match {
    case expressionVariable: ExpressionVariable[_] =>
      Some(expressionVariable.name, expressionVariable.arguments)
    case _ =>
      None
  }
}


