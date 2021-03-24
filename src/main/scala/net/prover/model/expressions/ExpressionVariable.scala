package net.prover.model.expressions

import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.old.OldParameterInserter
import net.prover.substitutionFinding.model.PossibleSubstitutions

import scala.reflect.ClassTag

abstract class ExpressionVariable[ExpressionType <: Expression : ClassTag] extends Expression with TypedExpression[ExpressionType] with ExpressionLenses[ExpressionType] { this: ExpressionType =>
  def index: Int
  def arguments: Seq[Term]
  val arity: Int = arguments.length

  def getMatch(other: Expression): Option[Seq[Expression]]
  def update(newArguments: Seq[Term]): ExpressionType

  override def structuralComplexity: Int = 0
  override def definitionalComplexity: Int = 0

  override def usedVariables: UsedVariables = (usedVariablesLens.set(Seq(UsedVariable(index, arguments.length)))(UsedVariables.empty) +: arguments.map(_.usedVariables)).foldTogether
  override def definitionUsages: DefinitionUsages = arguments.map(_.definitionUsages).foldTogether
  override def replaceDefinitions(expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition]): ExpressionType = {
    update(arguments.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }

  override def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, ExpressionType, Int, Seq[Int])] = {
    @scala.annotation.tailrec
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

  override def calculateApplicatives(
    baseArguments: Seq[Term],
    substitutions: PossibleSubstitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(ExpressionType, PossibleSubstitutions)] = {
    arguments.calculateApplicatives(baseArguments, substitutions, internalDepth, previousInternalDepth, externalDepth)
      .map(_.mapLeft(newArguments => update(newArguments.map(_.asInstanceOf[Term]))))
  }

  def serializationPrefix: String
  def serializationSymbol: String = serializationPrefix + index
  override def toString: String = serializationSymbol + (if (arguments.nonEmpty) "(" + arguments.map(_.toString).mkString(", ") + ")" else "")
  override def serialized: String = (if (arguments.nonEmpty) "with (" + arguments.map(_.serialized).mkString(" ") + ") " else "") + serializationSymbol
  override def serializedForHash: String = (if (arguments.nonEmpty) "with (" + arguments.map(_.serializedForHash).mkString(" ") + ") " else "") + serializationSymbol
}

object ExpressionVariable {
  def unapply(expression: Expression): Option[(Int, Seq[Term])] = expression match {
    case expressionVariable: ExpressionVariable[_] =>
      Some(expressionVariable.index, expressionVariable.arguments)
    case _ =>
      None
  }
}


