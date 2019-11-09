package net.prover.model.expressions

import net.prover.model.entries.ExpressionDefinition
import net.prover.model.proof.StepContext
import net.prover.model.{ExpressionParsingContext, Parser, Substitutions}

trait Expression extends TypedExpression[Expression]

trait TypedExpression[+ExpressionType <: Expression] { self: Expression =>
  def complexity: Int
  def definitionUsages: DefinitionUsages
  def referencedDefinitions: Set[ExpressionDefinition] = definitionUsages.map.keySet

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType]
  def replaceDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): ExpressionType

  def specify(
    targetArguments: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): ExpressionType
  def specify(
    targetArguments: Seq[Term],
    internalDepth: Int,
    externalDepth: Int
  ): ExpressionType = {
    specify(targetArguments.indices.zip(targetArguments).toMap, internalDepth, externalDepth).asInstanceOf[ExpressionType]
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

  def requiredSubstitutions: Substitutions.Required

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
    substitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Iterator[Substitutions]
  def calculateSubstitutions(other: Expression, stepContext: StepContext): Iterator[Substitutions] = {
    calculateSubstitutions(other, Substitutions.empty, 0, stepContext.externalDepth)
  }
  def calculateSubstitutions(other: Expression, substitutions: Substitutions, stepContext: StepContext): Iterator[Substitutions] = {
    calculateSubstitutions(other, substitutions, 0, stepContext.externalDepth)
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
    substitutions: Substitutions,
    stepContext: StepContext
  ): Option[ExpressionType] = {
    applySubstitutions(substitutions, 0, stepContext.externalDepth).asInstanceOf[Option[ExpressionType]]
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
    substitutions: Substitutions,
    internalDepth: Int,
    previousInternalDepth: Int,
    externalDepth: Int
  ): Iterator[(ExpressionType, Substitutions)]
  def calculateArguments(
    target: Expression,
    argumentsSoFar: Map[Int, Term],
    internalDepth: Int,
    externalDepth: Int
  ): Option[Map[Int, Term]]

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
    def requiredSubstitutions: Substitutions.Required = expressions.map(_.requiredSubstitutions).foldTogether
    def calculateSubstitutions(
      otherExpressions: Seq[Expression],
      substitutions: Substitutions,
      internalDepth: Int,
      externalDepth: Int
    ): Iterator[Substitutions] = {
      expressions.zipStrict(otherExpressions).toSeq.flatten
        .foldLeft(Iterator(substitutions)) { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(expression.calculateSubstitutions(otherExpression, _, internalDepth, externalDepth))
        }
    }
    def calculateApplicatives(
      arguments: Seq[Term],
      substitutions: Substitutions,
      internalDepth: Int,
      previousInternalDepth: Int,
      externalDepth: Int
    ): Iterator[(Seq[Expression], Substitutions)] = {
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
      internalDepth: Int,
      externalDepth: Int
    ): Option[Map[Int, Term]] = {
      expressions.zipStrict(targets).flatMap(_.foldLeft(Option(argumentsSoFar)) { case (argumentsOption, (expression, target)) =>
        for {
          arguments <- argumentsOption
          result <- expression.calculateArguments(target, arguments, internalDepth, externalDepth)
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
  }
}
