package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext

case class BinaryOperator(template: Term) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Term = {
    template.applySubstitutions(template.requiredSubstitutions.fill(Nil, Seq(left, right))).get
  }
  def unapply(term: Term)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = {
    for {
      substitutions <- template.calculateSubstitutions(term)
      Seq(left, right) <- template.requiredSubstitutions.terms.map { case (name, _) => substitutions.terms.get(name).map(_._2) }.traverseOption
    } yield (left, right)
  }
}
