package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}

case class Reversal(relation: BinaryStatement[Term], inference: Inference.Summary) {
  def assertionStep(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      relation(left, right),
      inference,
      Seq(Premise.Pending(relation(right, left))),
      Substitutions(terms = inference.requiredSubstitutions.terms.zip(Seq(right, left)).toMap))
  }
}

