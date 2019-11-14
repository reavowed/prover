package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}

case class Transitivity(relation: BinaryRelation, inference: Inference.Summary) {
  def assertionStep(left: Term, middle: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      relation(left, right),
      inference,
      Seq(Premise.Pending(relation(left, middle)), Premise.Pending(relation(middle, right))),
      Substitutions(terms = inference.requiredSubstitutions.terms.zip(Seq(left, middle, right)).toMap))
  }
}
