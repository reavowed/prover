package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Expression {
  def depth: Int
  def requiredSubstitutions: Substitutions.Required
  def calculateSubstitutions(other: Expression, substitutions: Substitutions): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Expression]

  def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions): Seq[(ExpressionFunction[Expression], Substitutions)]
  def makeApplicative: Option[ExpressionFunction[Expression]]
  def increaseDepth(additionalDepth: Int): ExpressionFunction[Expression]

  def replacePlaceholder(other: Expression): Expression
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
    Assertable.parser.tryOrElse(Objectable.parser)
  }

  implicit class ExpressionSeqOps(expressions: Seq[Expression]) {
    def requiredSubstitutions: Substitutions.Required = expressions.map(_.requiredSubstitutions).foldTogether
    def calculateSubstitutions(otherExpressions: Seq[Expression], substitutions: Substitutions): Seq[Substitutions] = {
      expressions.zipStrict(otherExpressions).toSeq.flatten
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(expression.calculateSubstitutions(otherExpression, _))
        }
    }
    def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions): Seq[(Seq[ExpressionFunction[Expression]], Substitutions)] = {
      expressions.foldLeft(Seq((Seq.empty[ExpressionFunction[Expression]], substitutions))) { case (predicatesAndSubstitutionsSoFar, expression) =>
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
