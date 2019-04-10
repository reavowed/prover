package net.prover.model.expressions

import net.prover.model.entries.ExpressionDefinition
import net.prover.model.{Parser, ExpressionParsingContext, Substitutions}

trait Expression extends TypedExpression[Expression]

trait TypedExpression[+ExpressionType <: Expression] { self: Expression =>
  def complexity: Int
  def referencedDefinitions: Set[ExpressionDefinition]

  def insertExternalParameters(numberOfParametersToInsert: Int, internalDepth: Int = 0): ExpressionType
  def removeExternalParameters(numberOfParametersToRemove: Int, internalDepth: Int = 0): Option[ExpressionType]

  def specify(
    targetArguments: Seq[Term],
    internalDepth: Int,
    externalDepth: Int
  ): ExpressionType

  /**
    * Specify, lazily substituting any argument used.
    * @param targetArguments the arguments to specify with
    * @param substitutions the substitutions to use
    * @param internalDepth The current scope depth inside the expression since we starting applying substitutions
    * @param externalDepth The depth of external scoped variables that might be referred to by this statement or substitutions
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
  ): Seq[Substitutions]

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

  /**
    * Calculate an applicative and a set of substitutions, such that when the applicative is applied to the given
    * arguments after substitution, this expression results.
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
  ): Seq[(ExpressionType, Substitutions)]

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
    ): Seq[Substitutions] = {
      expressions.zipStrict(otherExpressions).toSeq.flatten
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(expression.calculateSubstitutions(otherExpression, _, internalDepth, externalDepth))
        }
    }
    def calculateApplicatives(
      arguments: Seq[Term],
      substitutions: Substitutions,
      internalDepth: Int,
      previousInternalDepth: Int,
      externalDepth: Int
    ): Seq[(Seq[Expression], Substitutions)] = {
      expressions.foldLeft(Seq((Seq.empty[Expression], substitutions))) { case (predicatesAndSubstitutionsSoFar, expression) =>
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
    def applySubstitutions(
      substitutions: Substitutions,
      internalDepth: Int,
      externalDepth: Int
    ): Option[Seq[Expression]] = {
      expressions.map(_.applySubstitutions(substitutions, internalDepth, externalDepth)).traverseOption
    }
  }
}
