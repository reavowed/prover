package net.prover.model.definitions

import net.prover.model._
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.SubstitutionContext

case class BinaryRelation(template: Statement) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Statement = {
    template.applySubstitutions(Substitutions(terms = template.requiredSubstitutions.terms.zip(Seq(left, right)).toMap)).get
  }
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = {
    for {
      substitutions <- template.calculateSubstitutions(statement)
      Seq(left, right) <- template.requiredSubstitutions.terms.map(substitutions.terms.get).traverseOption
    } yield (left, right)
  }
}
