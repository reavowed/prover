package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.definitions.CompoundExpressionDefinition
import net.prover.model.proof.SubstitutionContext
import net.prover.model.{ExpressionParsingContext, Parser, UsedVariables}
import net.prover.substitutionFinding.model.PossibleSubstitutions

@JsonSerialize(using = classOf[ExpressionSerializer])
trait Expression extends TypedExpression[Expression]

trait TypedExpression[+ExpressionType <: Expression] {
  def structuralComplexity: Int
  def definitionalComplexity: Int
  lazy val complexity: (Int, Int) = (structuralComplexity, definitionalComplexity)
  def definitionUsages: DefinitionUsages
  def referencedDefinitions: Set[CompoundExpressionDefinition] = definitionUsages.map.keySet

  def usedVariables: UsedVariables
  def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, ExpressionType, Int, Seq[Int])]
  def getTerms()(implicit substitutionContext: SubstitutionContext): Seq[(Term, ExpressionType, Int, Seq[Int])] = getTerms(0, substitutionContext.externalDepth)
  def getPredicateForTerm(term: Term, depth: Int): ExpressionType

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType]
  def replaceDefinitions(expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition]): ExpressionType

  def specify(
    targetArguments: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType]
  def specify(
    targetArguments: Seq[Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType] = {
    specify(targetArguments.indices.zip(targetArguments).toMap, internalDepth, externalDepth)
  }
  def specify(targetArguments: Map[Int, Term])(implicit substitutionContext: SubstitutionContext): Option[ExpressionType] = {
    specify(targetArguments, 0, substitutionContext.externalDepth)
  }
  def specify(targetArguments: Seq[Term])(implicit substitutionContext: SubstitutionContext): Option[ExpressionType] = {
    specify(targetArguments.indices.zip(targetArguments).toMap)
  }

  def trySpecifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: PossibleSubstitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType]

  def tryApplySubstitutions(
    substitutions: PossibleSubstitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType]
  def tryApplySubstitutions(
    substitutions: PossibleSubstitutions)(
    implicit substitutionContext: SubstitutionContext
  ): Option[ExpressionType] = {
    tryApplySubstitutions(substitutions, 0, substitutionContext.externalDepth)
  }

  /**
    * Calculate an applicative and a set of substitutions, such that when the applicative is applied to the given
    * arguments after substitution, this expression results.
    *
    * Used in calculating substitutions for a function or predicate application - calling e.g.
    * `F(a).calculateSubstitutions(other)` will result in a call to `other.calculateApplicatives(Seq(a))`.
    *
    * @param internalDepth The current scope depth inside the expression since we starting calculating applicatives
    * @param previousInternalDepth The scope depth inside the expression at the point we started calculating applicatives
    * @param externalDepth The depth of external scoped variables that might be referred to by this statement
    */
  def calculateApplicatives(
    targetArguments: Seq[Term],
    substitutions: PossibleSubstitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(ExpressionType, PossibleSubstitutions)]
  def calculateApplicatives(
    targetArguments: Seq[Term],
    substitutions: PossibleSubstitutions)(
    implicit substitutionContext: SubstitutionContext
  ): Iterator[(ExpressionType, PossibleSubstitutions)] = {
    calculateApplicatives(targetArguments, substitutions, 0, 0, substitutionContext.externalDepth)
  }

  def renameBoundVariable(newName: String, index: Int, path: Seq[Int]): Option[ExpressionType] = None
  def findComponentPath(other: Expression): Option[Seq[Int]] = {
    if (this == other) {
      Some(Nil)
    } else {
      None
    }
  }
  def safeToString: String = toString
  def serialized: String
  def serializedForHash: String
}

object Expression {
  def parser(implicit parsingContext: ExpressionParsingContext): Parser[Expression] = {
    Statement.parser.tryOrElse(Term.parser)
  }

  implicit class ExpressionSeqOps(expressions: Seq[Expression]) {
    def usedVariables: UsedVariables = {
      expressions.map(_.usedVariables).foldTogether
    }
    def calculateApplicatives(
      arguments: Seq[Term],
      substitutions: PossibleSubstitutions,
      internalDepth: Int,
      previousInternalDepth: Int,
      externalDepth: Int
    ): Iterator[(Seq[Expression], PossibleSubstitutions)] = {
      expressions.iterator.foldLeft(Iterator((Seq.empty[Expression], substitutions))) { case (predicatesAndSubstitutionsSoFar, expression) =>
        for {
          (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
          (predicate, newSubstitutions) <- expression.calculateApplicatives(
            arguments,
            substitutionsSoFar,
            internalDepth,
            previousInternalDepth,
            externalDepth)
        } yield (predicatesSoFar :+ predicate, newSubstitutions)
      }
    }
    def tryApplySubstitutions(
      substitutions: PossibleSubstitutions,
      internalDepth: Int,
      externalDepth: Int
    ): Option[Seq[Expression]] = {
      expressions.map(_.tryApplySubstitutions(substitutions, internalDepth, externalDepth)).traverseOption
    }
  }
}

sealed trait ExpressionType[T <: Expression]
object ExpressionType {
  implicit object StatementType extends ExpressionType[Statement]
  implicit object TermType extends ExpressionType[Term]
}

private class ExpressionSerializer extends JsonSerializer[Expression] {
  override def serialize(value: Expression, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(value.serialized)
  }
}
