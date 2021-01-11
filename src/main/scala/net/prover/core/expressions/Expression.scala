package net.prover.core.expressions

sealed trait Expression

sealed trait Statement extends Expression
sealed trait Term extends Expression

sealed trait ExpressionVariable[TVariable <: ExpressionVariable[TVariable, TExpression], TExpression <: Expression] extends Expression { self: TExpression =>
  def index: Int
  def arguments: Seq[Term]
  def withNewArguments(newArguments: Seq[Term]): TVariable

  def arity: Int = arguments.length
  def matchOther(expression: Expression): Option[TVariable]
}
case class StatementVariable(index: Int, arguments: Seq[Term]) extends ExpressionVariable[StatementVariable, Statement] with Statement {
  override def withNewArguments(newArguments: Seq[Term]): StatementVariable = copy(arguments = newArguments)
  override def matchOther(expression: Expression): Option[StatementVariable] = expression.asOptionalInstanceOf[StatementVariable].filter(_.index == index)
}
object StatementVariable {
  def plain(index: Int): StatementVariable = StatementVariable(index, Nil)
}
case class TermVariable(index: Int, arguments: Seq[Term]) extends ExpressionVariable[TermVariable, Term] with Term {
  override def withNewArguments(newArguments: Seq[Term]): TermVariable = copy(arguments = newArguments)
  override def matchOther(expression: Expression): Option[TermVariable] = expression.asOptionalInstanceOf[TermVariable].filter(_.index == index)
}
object TermVariable {
  def plain(index: Int): TermVariable = TermVariable(index, Nil)
}

sealed trait CompoundExpression[TCompoundExpression <: CompoundExpression[TCompoundExpression, TExpression], TExpression <: Expression] {
  def definition: CompoundExpressionType
  def components: Seq[Expression]
  def boundVariableNames: Seq[String]
  def withNewComponents(newComponents: Seq[Expression]): TCompoundExpression
  def withNewBoundVariableNames(newBoundVariableNames: Seq[String]): TCompoundExpression

  def hasBoundVariables: Boolean = definition.hasBoundVariables
  def matchOther(expression: Expression): Option[TCompoundExpression]
}
case class CompoundStatement(definition: CompoundStatementType, components: Seq[Expression])(val boundVariableNames: Seq[String]) extends CompoundExpression[CompoundStatement, Statement] with Statement {
  override def withNewComponents(newComponents: Seq[Expression]): CompoundStatement = copy(components = newComponents)(boundVariableNames)
  override def withNewBoundVariableNames(newBoundVariableNames: Seq[String]): CompoundStatement = copy()(newBoundVariableNames)
  override def matchOther(expression: Expression): Option[CompoundStatement] = expression.asOptionalInstanceOf[CompoundStatement].filter(_.definition == definition)

}
case class CompoundTerm(definition: CompoundTermType, components: Seq[Expression])(val boundVariableNames: Seq[String])  extends CompoundExpression[CompoundTerm, Term] with Term  {
  override def withNewComponents(newComponents: Seq[Expression]): CompoundTerm = copy(components = newComponents)(boundVariableNames)
  override def withNewBoundVariableNames(newBoundVariableNames: Seq[String]): CompoundTerm = copy()(newBoundVariableNames)
  override def matchOther(expression: Expression): Option[CompoundTerm] = expression.asOptionalInstanceOf[CompoundTerm].filter(_.definition == definition)
}

case class Parameter(index: Int, level: Int) extends Term
