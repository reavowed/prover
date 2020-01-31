package net.prover.model.definitions

import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Step, StepContext, SubstitutionContext}
import net.prover.model._

case class Equality(relation: BinaryRelation, expansion: Expansion[Term], substitution: Substitution, reversal: Reversal[Term], transitivity: Transitivity[Term]) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Statement = relation(left, right)
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = relation.unapply(statement)

  def reversalRearrangementStep(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): RearrangementStep[Term] = {
    RearrangementStep(
      wrapper(right),
      Seq(reversal.assertionStep(left, right)) ++ expansion.assertionStepIfNecessary(left, right, wrapper),
      reversal.inference)
  }
}
