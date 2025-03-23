package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.proof.SubstitutionContext
import net.prover.model.{ExpressionParsingContext, Substitutions, UsedVariables}
import net.prover.parsing.Parser

@JsonSerialize(`using` = classOf[ExpressionSerializer])
trait Expression extends TypedExpression[Expression]

trait TypedExpression[+ExpressionType <: Expression] {
  def structuralComplexity: Int
  def definitionalComplexity: Int
  lazy val complexity: (Int, Int) = (structuralComplexity, definitionalComplexity)
  def definitionUsages: DefinitionUsages
  def referencedDefinitions: Set[ExpressionDefinition] = definitionUsages.map.keySet

  def usedVariables: UsedVariables
  def getTerms(internalDepth: Int, externalDepth: Int): Seq[(Term, ExpressionType, Int, Seq[Int])]
  def getTerms()(implicit substitutionContext: SubstitutionContext): Seq[(Term, ExpressionType, Int, Seq[Int])] = getTerms(0, substitutionContext.externalDepth)
  def getPredicateForTerm(term: Term, depth: Int): ExpressionType

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType]
  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): ExpressionType

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

  /**
    * Specify, lazily substituting any argument used.
    *
    * @param targetArguments       the arguments to specify with
    * @param substitutions         the substitutions to use
    * @param internalDepth         The current scope depth inside the expression since we starting specifying.
    * @param previousInternalDepth The scope depth inside the expression before we started specifying.  Specified predicates
    *                              may not refer to parameters in this range - they must be passed in through the arguments.
    * @param externalDepth         The depth of external scoped variables that might be referred to by this statement or substitutions
    * @return
    */
  def specifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType]
  def trySpecifyWithSubstitutions(
    targetArguments: Seq[Term],
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType]

  /**
    * Calculate all valid substitutions that can be applied to this to result in other,
    * given a set of conditions.
    *
    * @param other The target expression to match
    * @param substitutions A set of existing substitutions to preserve
    * @param internalDepth The current scope depth inside the expression since we starting calculating substitutions
    * @param externalDepth The depth of external scoped variables that might be referred to by the other statement
    * @return A sequence of valid substitutions
    */
  def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Substitutions.Possible]
  def calculateSubstitutions(other: Expression)(implicit substitutionContext: SubstitutionContext): Option[Substitutions.Possible] = {
    calculateSubstitutions(other, Substitutions.Possible.empty)
  }
  def calculateSubstitutions(other: Expression, substitutions: Substitutions.Possible)(implicit substitutionContext: SubstitutionContext): Option[Substitutions.Possible] = {
    calculateSubstitutions(other, substitutions, 0, substitutionContext.externalDepth)
  }

  /**
    * Apply the given substitutions to this statement.
    * @param internalDepth The current scope depth inside the expression since we starting applying substitutions
    * @param externalDepth The depth of external scoped variables that might be referred to by substitutions
    * @return
    */
  def applySubstitutions(
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType]
  def applySubstitutions(
    substitutions: Substitutions)(
    implicit substitutionContext: SubstitutionContext
  ): Option[ExpressionType] = {
    applySubstitutions(substitutions, 0, substitutionContext.externalDepth)
  }
  def tryApplySubstitutions(
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    externalDepth: Int
  ): Option[ExpressionType]
  def tryApplySubstitutions(
    substitutions: Substitutions.Possible)(
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
    substitutions: Substitutions.Possible,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(ExpressionType, Substitutions.Possible)]
  def calculateApplicatives(
    targetArguments: Seq[Term],
    substitutions: Substitutions.Possible)(
    implicit substitutionContext: SubstitutionContext
  ): Iterator[(ExpressionType, Substitutions.Possible)] = {
    calculateApplicatives(targetArguments, substitutions, 0, 0, substitutionContext.externalDepth)
  }

  def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term],
    previousInternalDepth: Int,
    internalDepth: Int,
    externalDepth: Int
  ): Option[Map[Int, Term]]
  def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term])(
    implicit substitutionContext: SubstitutionContext
  ): Option[Map[Int, Term]] = calculateArguments(target, argumentsSoFar, 0, 0, substitutionContext.externalDepth)

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
    def calculateSubstitutions(
      otherExpressions: Seq[Expression],
      substitutions: Substitutions.Possible,
      internalDepth: Int,
      externalDepth: Int
    ): Option[Substitutions.Possible] = {
      expressions.zipStrict(otherExpressions)
        .flatMap(_.foldLeft(Option(substitutions))  { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(s => expression.calculateSubstitutions(otherExpression, s, internalDepth, externalDepth))
        })
    }
    def calculateApplicatives(
      arguments: Seq[Term],
      substitutions: Substitutions.Possible,
      internalDepth: Int,
      previousInternalDepth: Int,
      externalDepth: Int
    ): Iterator[(Seq[Expression], Substitutions.Possible)] = {
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
    def calculateArguments(
      targets: Seq[Expression],
      argumentsSoFar: Map[Int, Term],
      previousInternalDepth: Int,
      internalDepth: Int,
      externalDepth: Int
    ): Option[Map[Int, Term]] = {
      expressions.zipStrict(targets).flatMap(_.foldLeft(Option(argumentsSoFar)) { case (argumentsOption, (expression, target)) =>
        for {
          arguments <- argumentsOption
          result <- expression.calculateArguments(target, arguments, previousInternalDepth, internalDepth, externalDepth)
        } yield result
      })
    }
    def applySubstitutions(
      substitutions: Substitutions,
      internalDepth: Int,
      externalDepth: Int
    ): Option[Seq[Expression]] = {
      expressions.map(_.applySubstitutions(substitutions, internalDepth, externalDepth)).traverseOption
    }
    def tryApplySubstitutions(
      substitutions: Substitutions.Possible,
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
