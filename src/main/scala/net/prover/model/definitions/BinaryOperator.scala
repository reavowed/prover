package net.prover.model.definitions

import net.prover.model.Substitutions
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext

case class BinaryOperator(template: Term) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Term = {
    template.applySubstitutions(Substitutions(terms = template.requiredSubstitutions.terms.zip(Seq(left, right)).toMap)).get
  }
  def unapply(term: Term)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = {
    for {
      substitutions <- template.calculateSubstitutions(term)
      Seq(left, right) <- template.requiredSubstitutions.terms.map(substitutions.terms.get).traverseOption
    } yield (left, right)
  }
}
