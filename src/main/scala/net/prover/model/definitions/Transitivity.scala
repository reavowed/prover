package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Expression
import net.prover.model.proof.{Premise, Step, StepContext, SubstitutionContext}

case class Transitivity[TComponent <: Expression](statement: BinaryJoiner[TComponent], inference: Inference.Summary) {
  def assertionStep(left: TComponent, middle: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      statement(left, right),
      inference,
      Seq(Premise.Pending(statement(left, middle)), Premise.Pending(statement(middle, right))),
      statement.fillRequiredSubstitutions(inference.requiredSubstitutions, Seq(left, middle, right)))
  }

  def addToRearrangement(base: TComponent, rearrangementSteps: Seq[RearrangementStep[TComponent]])(implicit stepContext: StepContext): Seq[Step] = {
    rearrangementSteps.headAndTailOption.toSeq.flatMap { case (head, tail) =>
      head.elidedStep.toSeq ++ tail.flatMapFold(head.result) { case (previousTerm, tailStep) => (
        tailStep.result,
        tailStep.elidedStep.toSeq :+ assertionStep(base, previousTerm, tailStep.result))
      }._2
    }
  }
}
