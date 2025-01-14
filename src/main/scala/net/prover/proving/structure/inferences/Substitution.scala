package net.prover.proving.structure.inferences

import net.prover.model.definitions.Wrapper
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Premise, Step, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.proving.structure.statements.BinaryJoiner

case class Substitution(relation: BinaryJoiner[Term], inference: Inference.Summary) {
  def assertionStep(premiseTerm: Term, targetTerm: Term, wrapper: Wrapper[Term, Statement])(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    Step.AssertionStep(
      wrapper(targetTerm),
      inference,
      Seq(
        Premise.Pending(relation(premiseTerm, targetTerm)),
        Premise.Pending(wrapper(premiseTerm))),
      Substitutions(Seq(wrapper.template), Seq(premiseTerm, targetTerm)))
  }
}
