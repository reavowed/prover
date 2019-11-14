package net.prover.model.definitions

import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Step, StepContext, SubstitutionContext}
import net.prover.model._

case class Equality(relation: BinaryRelation, expansion: Expansion, substitution: Substitution, reversal: Reversal, transitivity: Transitivity) {
  def apply(left: Term, right: Term)(implicit substitutionContext: SubstitutionContext): Statement = relation(left, right)
  def unapply(statement: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Term, Term)] = relation.unapply(statement)

  def reversalRearrangementStep(left: Term, right: Term, wrapper: Wrapper[Term, Term])(implicit substitutionContext: SubstitutionContext): RearrangementStep = {
    RearrangementStep(
      wrapper(right),
      Seq(reversal.assertionStep(left, right)) ++ expansion.assertionStepIfNecessary(left, right, wrapper),
      reversal.inference)
  }

  def addTransitivityToRearrangement(base: Term, rearrangementSteps: Seq[RearrangementStep])(implicit stepContext: StepContext): Seq[Step] = {
    rearrangementSteps.headAndTailOption.toSeq.flatMap { case (head, tail) =>
      head.elidedStep.toSeq ++ tail.flatMapFold(head.resultingTerm) { case (previousTerm, tailStep) => (
        tailStep.resultingTerm,
        tailStep.elidedStep.toSeq :+ transitivity.assertionStep(base, previousTerm, tailStep.resultingTerm))
      }._2
    }
  }
}
