package net.prover.model.definitions

import net.prover.model.ProvingContext
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext

case class RearrangeableOperator(
  operator: BinaryOperator,
  commutativity: Commutativity,
  associativity: Associativity,
  leftIdentities: Seq[LeftIdentity],
  rightIdentities: Seq[RightIdentity],
  leftAbsorbers: Seq[LeftAbsorber],
  rightAbsorbers: Seq[RightAbsorber],
  inverses: Seq[DoubleSidedInverse]
) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(left, right)
  def unapply(term: Term)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = operator.unapply(term)
}

object RearrangeableOperator {
  def unapply(term: Term)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Option[(RearrangeableOperator, Term, Term)] = provingContext.rearrangeableOperators.mapFind { o =>
    o.unapply(term).map { case (l, r) => (o, l, r) }
  }
}
