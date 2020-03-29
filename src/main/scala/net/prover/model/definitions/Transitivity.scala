package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Expression
import net.prover.model.proof.{Premise, Step, StepContext, SubstitutionContext}

case class Transitivity[TComponent <: Expression](firstPremiseJoiner: BinaryJoiner[TComponent], secondPremiseJoiner: BinaryJoiner[TComponent], resultJoiner: BinaryJoiner[TComponent], inference: Inference.Summary) {
  def assertionStep(left: TComponent, middle: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      resultJoiner(left, right),
      inference,
      Seq(Premise.Pending(firstPremiseJoiner(left, middle)), Premise.Pending(secondPremiseJoiner(middle, right))),
      resultJoiner.fillRequiredSubstitutions(inference.requiredSubstitutions, Seq(left, middle, right)))
  }

  def addToRearrangement(base: TComponent, rearrangementSteps: Seq[RearrangementStep[TComponent]])(implicit stepContext: StepContext): Seq[Step] = {
    rearrangementSteps.headAndTailOption.toSeq.flatMap { case (head, tail) =>
      head.elidedStep.toSeq ++ tail.flatMapFold(head.result) { case (previousTerm, tailStep) => (
        tailStep.result,
        tailStep.elidedStep.toSeq :+ assertionStep(base, previousTerm, tailStep.result))
      }._2
    }
  }

  def isTransitivityForJoiner(joiner: BinaryJoiner[_]): Boolean = {
    firstPremiseJoiner == joiner && secondPremiseJoiner == joiner && resultJoiner == joiner
  }
}
