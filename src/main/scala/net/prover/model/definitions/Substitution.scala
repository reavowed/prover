package net.prover.model.definitions

import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Premise, Step, SubstitutionContext}

case class Substitution(relation: BinaryJoiner[Term], inference: Inference.Summary) {
  def assertionStep(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Statement])(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      wrapper(targetTerm),
      inference,
      Seq(
        Premise.Pending(relation(premiseTerm, targetTerm)),
        Premise.Pending(wrapper(premiseTerm))),
      Substitutions(Seq(wrapper.template), Seq(premiseTerm, targetTerm)))
  }
}
