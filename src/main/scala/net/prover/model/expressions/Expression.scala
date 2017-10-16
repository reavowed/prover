package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Expression {
  def depth: Int
  def increaseDepth(additionalDepth: Int, insertionPoint: Int): Expression
  def reduceDepth(difference: Int, insertionPoint: Int): Option[Expression]
  def specify(targetArguments: ArgumentList): Expression
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions,
    outerDepth: Int
  ): Option[Expression]

  def requiredSubstitutions: Substitutions.Required
  def calculateSubstitutions(other: Expression, substitutions: Substitutions): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Expression]
  def calculateApplicatives(targetArguments: ArgumentList, substitutions: Substitutions): Seq[(Expression, Substitutions)]

  def condense(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ): Option[(Substitutions, Substitutions)] = {
    condenseOneWay(other, thisSubstitutions, otherSubstitutions) orElse
      other.condenseOneWay(this, otherSubstitutions, thisSubstitutions).map(_.reverse)
  }
  protected def condenseOneWay(
    other: Expression,
    thisSubstitutions: Substitutions,
    otherSubstitutions: Substitutions
  ): Option[(Substitutions, Substitutions)] = {
    for {
      thisSubstituted <- applySubstitutions(thisSubstitutions)
      updatedOtherSubstitutions <- other.calculateSubstitutions(thisSubstituted, otherSubstitutions).headOption
    } yield (thisSubstitutions, updatedOtherSubstitutions)
  }

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
    def calculateSubstitutions(otherExpressions: Seq[Expression], substitutions: Substitutions): Seq[Substitutions] = {
      expressions.zipStrict(otherExpressions).toSeq.flatten
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(expression.calculateSubstitutions(otherExpression, _))
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
