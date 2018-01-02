package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Expression {
  def depth: Int
  def increaseDepth(additionalDepth: Int, insertionPoint: Int): Expression
  def reduceDepth(difference: Int, insertionPoint: Int): Option[Expression]
  def specify(targetArguments: ArgumentList): Expression

  /**
    * Specify, lazily substituting any argument used.
    * @param targetArguments the arguments to specify with
    * @param substitutions the substitutions to use
    * @return
    */
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions
  ): Option[Expression]

  def requiredSubstitutions: Substitutions.Required

  /**
    * Calculate all valid substitutions that can be applied to this to result in other,
    * given a set of conditions.
    *
    * this.depth + substitutions.depth = other.depth
    *
    * @param other The target expression to match
    * @param substitutions A set of existing substitutions to preserve
    * @param applicativeHints A sequence of requirements on predicate application variables
    * @param structuralHints A sequence of requirements on non-predicate variables
    * @return A sequence of valid substitutions
    */
  def calculateSubstitutions(
    other: Expression,
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Seq[Substitutions]

  def applySubstitutions(substitutions: Substitutions): Option[Expression]

  /**
    * Calculate an applicative and a set of substitutions, such that when the applicative is applied to the given
    * arguments after substitution, this expression results.
    */
  def calculateApplicatives(targetArguments: ArgumentList, substitutions: Substitutions): Seq[(Expression, Substitutions)]

  def condense(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Seq[(Substitutions, Substitutions, Seq[(Substitutions, ArgumentList)], Seq[Substitutions])] = {
    val forward = condenseForward(other, thisSubstitutions, otherSubstitutions, applicativeHints, structuralHints)
    if (forward.nonEmpty)
      forward
    else
      other.condenseReverse(this, otherSubstitutions, thisSubstitutions).map(t => (t._2, t._1, t._3, t._4))
  }
  protected def condenseForward(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Seq[(Substitutions, Substitutions, Seq[(Substitutions, ArgumentList)], Seq[Substitutions])] = {
    (for {
      thisSubstituted <- applySubstitutions(thisSubstitutions).toSeq
      updatedOtherSubstitutions <- other.calculateSubstitutions(
        thisSubstituted,
        otherSubstitutions,
        applicativeHints,
        structuralHints)
    } yield (thisSubstitutions, updatedOtherSubstitutions))
      .map(t => (t._1, t._2, Nil, Nil))
  }
  protected def condenseReverse(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ): Seq[(Substitutions, Substitutions, Seq[(Substitutions, ArgumentList)], Seq[Substitutions])] = {
    (for {
      thisSubstituted <- applySubstitutions(thisSubstitutions).toSeq
      updatedOtherSubstitutions <- other.calculateSubstitutions(thisSubstituted, otherSubstitutions, Nil, Nil)
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
      applicativeHints: Seq[(Substitutions, ArgumentList)],
      structuralHints: Seq[Substitutions]
    ): Seq[Substitutions] = {
      expressions.zipStrict(otherExpressions).toSeq.flatten
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(expression.calculateSubstitutions(otherExpression, _, applicativeHints, structuralHints))
        }
    }
    def calculateApplicatives(arguments: ArgumentList, substitutions: Substitutions): Seq[(Seq[Expression], Substitutions)] = {
      expressions.foldLeft(Seq((Seq.empty[Expression], substitutions))) { case (predicatesAndSubstitutionsSoFar, expression) =>
        for {
          (predicatesSoFar, substitutionsSoFar) <- predicatesAndSubstitutionsSoFar
          (predicate, newSubstitutions) <- expression.calculateApplicatives(
            arguments,
            substitutionsSoFar)
        } yield (predicatesSoFar :+ predicate, newSubstitutions)
      }
    }
    def applySubstitutions(substitutions: Substitutions): Option[Seq[Expression]] = {
      expressions.map(_.applySubstitutions(substitutions)).traverseOption
    }
  }
}
