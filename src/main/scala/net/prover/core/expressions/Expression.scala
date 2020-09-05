package net.prover.core.expressions

sealed trait Expression

sealed trait Statement extends Expression
sealed trait Term extends Expression

sealed trait ExpressionVariable[T <: ExpressionVariable[T]] {
  def index: Int
  def arguments: Seq[Term]
  def withNewArguments(newArguments: Seq[Term]): T
}
case class StatementVariable(index: Int, arguments: Seq[Term]) extends ExpressionVariable[StatementVariable] with Statement {
  override def withNewArguments(newArguments: Seq[Term]): StatementVariable = copy(arguments = newArguments)
}
case class TermVariable(index: Int, arguments: Seq[Term]) extends ExpressionVariable[TermVariable] with Term {
  override def withNewArguments(newArguments: Seq[Term]): TermVariable = copy(arguments = newArguments)
}

sealed trait DefinedExpression[T <: DefinedExpression[T]] {
  def components: Seq[Expression]
  def boundVariableNames: Seq[String]
  def withNewComponents(newComponents: Seq[Expression]): T

  def hasBinding: Boolean = boundVariableNames.nonEmpty
}
case class DefinedStatement(definition: StatementDefinition, components: Seq[Expression])(val boundVariableNames: Seq[String]) extends DefinedExpression[DefinedStatement] with Statement {
  override def withNewComponents(newComponents: Seq[Expression]): DefinedStatement = copy(components = newComponents)(boundVariableNames)
}
case class DefinedTerm(definition: TermDefinition, components: Seq[Expression])(val boundVariableNames: Seq[String])  extends DefinedExpression[DefinedTerm] with Term  {
  override def withNewComponents(newComponents: Seq[Expression]): DefinedTerm = copy(components = newComponents)(boundVariableNames)
}

case class Parameter(index: Int, level: Int) extends Term
