package net.prover.proving.structure.statements

import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.SubstitutionContext

case class BinaryRelationStatement(relation: BinaryRelation, left: Term, right: Term)(val baseStatement: Statement) {
  def withNewRight(newRight: Term)(implicit substitutionContext: SubstitutionContext): BinaryRelationStatement = BinaryRelationStatement.construct(relation, left, newRight)
}

object BinaryRelationStatement {
  def construct(relation: BinaryRelation, left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): BinaryRelationStatement = {
    BinaryRelationStatement(relation, left, right)(relation(left, right))
  }
}
