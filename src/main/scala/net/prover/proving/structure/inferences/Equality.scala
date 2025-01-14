package net.prover.proving.structure.inferences

import net.prover.model.definitions.{RearrangementStep, Wrapper}
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.SubstitutionContext
import net.prover.proving.structure.statements.BinaryRelation

case class Equality(relation: BinaryRelation, expansion: Expansion[Term], substitution: Substitution, reversal: Reversal[Term], transitivity: Transitivity[Term]) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Statement = relation(left, right)
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = relation.unapply(statement)

  def reversalRearrangementStep[T <: Expression](left: Term, right: Term, wrapper: Wrapper[Term, T], expansion: Expansion[T])(implicit substitutionContext: SubstitutionContext): RearrangementStep[T] = {
    RearrangementStep[T](
      wrapper(right),
      Seq(reversal.assertionStep(left, right)) ++ expansion.assertionStepIfNecessary(left, right, wrapper),
      reversal.inference)
  }
}
