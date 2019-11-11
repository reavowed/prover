package net.prover.model

import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Premise, Step, StepContext}

case class TransitivityDefinition(symbol: String, template: Statement, inference: Inference.Summary) {
  def splitStatement(statement: Statement)(implicit stepContext: StepContext): Option[(Term, Term)] = {
    for {
      substitutions <- template.calculateSubstitutions(statement, stepContext)
      Seq(lhs, rhs) <- template.requiredSubstitutions.terms.map(substitutions.terms.get).traverseOption
    } yield (lhs, rhs)
  }
  def joinTerms(left: Term, right: Term)(implicit stepContext: StepContext): Statement = {
    template.applySubstitutions(Substitutions(terms = template.requiredSubstitutions.terms.zip(Seq(left, right)).toMap), stepContext).get
  }
  def assertionStep(left: Term, middle: Term, right: Term)(implicit stepContext: StepContext): Step.Assertion = {
    Step.Assertion(
      joinTerms(left, right),
      inference,
      Seq(Premise.Pending(joinTerms(left, middle)), Premise.Pending(joinTerms(middle, right))),
      Substitutions(terms = inference.requiredSubstitutions.terms.zip(Seq(left, middle, right)).toMap))
  }
}
