package net.prover.proving.structure.inferences

import net.prover.model.definitions.RearrangementStep
import net.prover.model.expressions.Expression
import net.prover.model.proof._
import net.prover.model.{Inference, ProvingContext}
import net.prover.proving.structure.statements.BinaryJoiner

case class Transitivity[TComponent <: Expression](
  firstPremiseJoiner: BinaryJoiner[TComponent],
  secondPremiseJoiner: BinaryJoiner[TComponent],
  resultJoiner: BinaryJoiner[TComponent],
  inference: Inference.Summary
) {
  def assertionStep(left: TComponent, middle: TComponent, right: TComponent)(implicit substitutionContext: SubstitutionContext): Step.AssertionStep = {
    Step.AssertionStep(
      resultJoiner(left, right),
      inference,
      Seq(Premise.Pending(firstPremiseJoiner(left, middle)), Premise.Pending(secondPremiseJoiner(middle, right))),
      resultJoiner.fillSubstitutions(Seq(left, middle, right)))
  }

  def addToRearrangement(base: TComponent, rearrangementSteps: Seq[RearrangementStep[TComponent]])(implicit substitutionContext: SubstitutionContext): Seq[Step] = {
    rearrangementSteps match {
      case head +: tail =>
        head.elidedStep.toSeq ++ foldIntoRearrangementSteps(base, head.result, tail)
      case Nil =>
        Nil
    }
  }

  def foldIntoRearrangementSteps(baseLhs: TComponent, initialRhs: TComponent, rearrangementSteps: Seq[RearrangementStep[TComponent]])(implicit substitutionContext: SubstitutionContext): Seq[Step] = {
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
  def addToRearrangement[TComponent <: Expression](
    base: TComponent,
    joiner: BinaryJoiner[TComponent],
    rearrangementSteps: Seq[RearrangementStep[TComponent]])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[Seq[Step]] = {
    rearrangementSteps match {
      case Nil =>
        Some(Nil)
      case Seq(single) =>
        Some(single.elidedStep.toSeq)
      case head +: tail =>
        provingContext.transitivities.ofType[Transitivity[TComponent]]
          .find(_.isTransitivityForJoiner(joiner))
          .map(transitivity => head.elidedStep.toSeq ++ transitivity.foldIntoRearrangementSteps(base, head.result, tail))
    }
  }
}
