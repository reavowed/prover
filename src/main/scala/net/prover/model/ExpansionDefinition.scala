package net.prover.model

import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Premise, Step, StepContext}

case class ExpansionDefinition(template: Statement, inference: Inference.Summary) {
  def joinTerms(left: Term, right: Term)(implicit stepContext: StepContext): Statement = {
    template.applySubstitutions(Substitutions(terms = template.requiredSubstitutions.terms.zip(Seq(left, right)).toMap), stepContext).get
  }
  def assertionStep(left: Term, right: Term, function: Term)(implicit stepContext: StepContext): Step.Assertion = {
    val expandedLeft = function.specify(Seq(left), 0, stepContext.externalDepth)
    val expandedRight = function.specify(Seq(right), 0, stepContext.externalDepth)
    Step.Assertion(
      joinTerms(expandedLeft, expandedRight),
      inference,
      Seq(Premise.Pending(joinTerms(left, right))),
      Substitutions(
        terms = inference.requiredSubstitutions.terms.zip(Seq(left, right)).toMap,
        functions = inference.requiredSubstitutions.functions.zip(Seq(function)).toMap))
  }
}
