package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Expression
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext, SubstitutionContext}

import scala.util.Try

case class Transitivity[TComponent <: Expression](firstPremiseJoiner: BinaryJoiner[TComponent], secondPremiseJoiner: BinaryJoiner[TComponent], resultJoiner: BinaryJoiner[TComponent], inference: Inference.Summary) {
  def assertionStep(left: TComponent, middle: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.Assertion = {
    Step.Assertion(
      resultJoiner(left, right),
      inference,
      Seq(Premise.Pending(firstPremiseJoiner(left, middle)), Premise.Pending(secondPremiseJoiner(middle, right))),
      resultJoiner.fillRequiredSubstitutions(inference.requiredSubstitutions, Seq(left, middle, right)))
  }

  def addToRearrangement(base: TComponent, rearrangementSteps: Seq[RearrangementStep[TComponent]])(implicit stepContext: StepContext): Seq[Step] = {
    rearrangementSteps match {
      case head +: tail =>
        head.elidedStep.toSeq ++ foldIntoRearrangementSteps(base, head.result, tail)
      case Nil =>
        Nil
    }
  }

  def foldIntoRearrangementSteps(baseLhs: TComponent, initialRhs: TComponent, rearrangementSteps: Seq[RearrangementStep[TComponent]])(implicit stepContext: StepContext): Seq[Step] = {
    rearrangementSteps.flatMapFold(initialRhs) { case (currentRhs, tailStep) => (
      tailStep.result,
      tailStep.elidedStep.toSeq :+ assertionStep(baseLhs, currentRhs, tailStep.result))
    }._2
  }

  def isTransitivityForJoiner(joiner: BinaryJoiner[_]): Boolean = {
    firstPremiseJoiner == joiner && secondPremiseJoiner == joiner && resultJoiner == joiner
  }
}

object Transitivity {
  def addToRearrangement[TComponent <: Expression](base: TComponent, joiner: BinaryJoiner[TComponent], rearrangementSteps: Seq[RearrangementStep[TComponent]])(implicit stepProvingContext: StepProvingContext): Option[Seq[Step]] = {
    rearrangementSteps match {
      case Nil =>
        Some(Nil)
      case Seq(single) =>
        Some(single.elidedStep.toSeq)
      case head +: tail =>
        stepProvingContext.provingContext.transitivities.ofType[Transitivity[TComponent]]
          .find(_.isTransitivityForJoiner(joiner))
          .map(transitivity => head.elidedStep.toSeq ++ transitivity.foldIntoRearrangementSteps(base, head.result, tail))
    }
  }
}
