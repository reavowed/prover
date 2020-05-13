package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext

case class RearrangeableOperator(operator: BinaryOperator, commutativity: Commutativity, associativity: Associativity) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(left, right)
  def unapply(term: Term)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = operator.unapply(term)
}
