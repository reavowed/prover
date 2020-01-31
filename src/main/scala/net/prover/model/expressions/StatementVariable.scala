package net.prover.model.expressions

import net.prover.model._

case class StatementVariable(name: String, arguments: Seq[Term]) extends ExpressionVariable[Statement] with Substitutions.Lenses.ForStatements with Statement {
  def getMatch(other: Expression): Option[Seq[Expression]] = other match {
    case StatementVariable(`name`, otherArguments) => Some(otherArguments)
    case _ => None
  }
  def update(newArguments: Seq[Term]): StatementVariable = StatementVariable(name, newArguments)
}

object StatementVariable {
  def apply(name: String): StatementVariable = StatementVariable(name, Nil)
}
