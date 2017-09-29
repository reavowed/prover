package net.prover.model.expressions

import net.prover.model.{Parser, ParsingContext, Substitutions}

trait Expression {
  def boundVariables: Set[Int]
  def requiredSubstitutions: Substitutions.Required
  def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int): Seq[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Expression]
  def replacePlaceholder(other: Expression): Expression
  def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int): Seq[(ExpressionFunction[Expression], Substitutions)]
  def makeApplicative(argument: Term): Option[Expression]
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
    def calculateSubstitutions(
      otherExpressions: Seq[Expression],
      substitutions: Substitutions,
      boundVariableCount: Int
    ): Seq[Substitutions] = {
      expressions.zipStrict(otherExpressions).toSeq.flatten
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (expression, otherExpression)) =>
          substitutionsSoFar.flatMap(expression.calculateSubstitutions(otherExpression, _, boundVariableCount))
        }
    }
  }
}
