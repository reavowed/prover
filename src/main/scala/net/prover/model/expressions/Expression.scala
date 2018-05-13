package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Expression extends TypedExpression[Expression]

trait TypedExpression[+ExpressionType <: Expression] { self: Expression =>
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
    * @param applicativeHints A sequence of requirements non predicate application variables
    * @param structuralHints A sequence of requirements on non-predicate variables
    * @param internalDepth The current scope depth inside the expression since we starting calculating substitutions
    * @param externalDepth The depth of external scoped variables that might be referred to by the other statement
    * @return A sequence of valid substitutions
    */
  def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, Seq[Term])],
    structuralHints: Seq[Substitutions],
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

  def condense(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, Seq[Term])],
    structuralHints: Seq[Substitutions],
    internalDepth: Int,
    externalDepth: Int
  ): Seq[(Substitutions, Substitutions, Seq[(Substitutions, Seq[Term])], Seq[Substitutions])] = {
    val forward = condenseForward(other, thisSubstitutions, otherSubstitutions, applicativeHints, structuralHints, internalDepth, externalDepth)
    if (forward.nonEmpty)
      forward
    else
      other.condenseReverse(this, otherSubstitutions, thisSubstitutions, internalDepth, externalDepth).map(t => (t._2, t._1, t._3, t._4))
  }
  protected def condenseForward(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, Seq[Term])],
    structuralHints: Seq[Substitutions],
    internalDepth: Int,
    externalDepth: Int
  ): Seq[(Substitutions, Substitutions, Seq[(Substitutions, Seq[Term])], Seq[Substitutions])] = {
    (for {
      thisSubstituted <- applySubstitutions(thisSubstitutions, internalDepth, externalDepth).toSeq
      updatedOtherSubstitutions <- other.calculateSubstitutions(
        thisSubstituted,
        otherSubstitutions,
        applicativeHints,
        structuralHints,
        internalDepth,
        externalDepth)
    } yield (thisSubstitutions, updatedOtherSubstitutions))
      .map(t => (t._1, t._2, Nil, Nil))
  }
  protected def condenseReverse(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    internalDepth: Int,
    externalDepth: Int
  ): Seq[(Substitutions, Substitutions, Seq[(Substitutions, Seq[Term])], Seq[Substitutions])] = {
    (for {
      thisSubstituted <- applySubstitutions(thisSubstitutions, internalDepth, externalDepth).toSeq
      updatedOtherSubstitutions <- other.calculateSubstitutions(thisSubstituted, otherSubstitutions, Nil, Nil, internalDepth, externalDepth)
    } yield (thisSubstitutions, updatedOtherSubstitutions))
      .map(t => (t._1, t._2, Nil, Nil))
  }

  def matchesStructure(other: Expression): Boolean
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
  def parser(implicit parsingContext: ParsingContext): Parser[Expression] = {
    Statement.parser.tryOrElse(Term.parser)
  }

  implicit class ExpressionSeqOps(expressions: Seq[Expression]) {
    def requiredSubstitutions: Substitutions.Required = expressions.map(_.requiredSubstitutions).foldTogether
    def calculateSubstitutions(
      otherExpressions: Seq[Expression],
      substitutions: Substitutions,
      applicativeHints: Seq[(Substitutions, Seq[Term])],
      structuralHints: Seq[Substitutions],
      internalDepth: Int,
      externalDepth: Int
    ): Seq[Substitutions] = {
      expressions.zipStrict(otherExpressions).toSeq.flatten
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(expression.calculateSubstitutions(otherExpression, _, applicativeHints, structuralHints, internalDepth, externalDepth))
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
