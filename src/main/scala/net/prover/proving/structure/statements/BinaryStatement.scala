package net.prover.proving.structure.statements

import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.SubstitutionContext

sealed trait BinaryStatement[TComponent <: Expression] {
  def joiner: BinaryJoiner[TComponent]
  def left: TComponent
  def right: TComponent
  def baseStatement: Statement
}
object BinaryStatement {
  def unapply[TComponent <: Expression](binaryStatement: BinaryStatement[TComponent]): Option[(BinaryJoiner[TComponent], TComponent, TComponent)] = {
    Some((binaryStatement.joiner, binaryStatement.left, binaryStatement.right))
  }
}

case class BinaryConnectiveStatement(connective: BinaryConnective, left: Statement, right: Statement)(val baseStatement: Statement) extends BinaryStatement[Statement] {
  override def joiner: BinaryConnective = connective
}

case class BinaryRelationStatement(relation: BinaryRelation, left: Term, right: Term)(val baseStatement: Statement) extends BinaryStatement[Term] {
  override def joiner: BinaryRelation = relation
  def withNewRight(newRight: Term)(implicit substitutionContext: SubstitutionContext): BinaryRelationStatement = BinaryRelationStatement.construct(relation, left, newRight)
}
object BinaryRelationStatement {
  def construct(relation: BinaryRelation, left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): BinaryRelationStatement = {
    BinaryRelationStatement(relation, left, right)(relation(left, right))
  }
}
