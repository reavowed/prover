package net.prover.proving.stepReplacement

import net.prover.controllers.ChainingMethods
import net.prover.entries.StepsWithContext
import net.prover.model.expressions.{Expression, Statement, Term}
import net.prover.model.proof.{Step, StepReference}
import net.prover.theorems.GetReferencedLines

object SplitPrecedingStepsBeforeChain {
  def apply(before: Seq[Step], after: Seq[Step], outerPath: Seq[Int])(implicit stepsWithContext: StepsWithContext): (Seq[Step], Seq[Step]) = {
    def splitPrecedingStepsWhileTransitiveGeneric[T <: Expression : ChainingMethods]: Option[(Seq[Step], Seq[Step])] = {
      @scala.annotation.tailrec
      def removeWhileTransitive(currentBefore: Seq[Step], currentStep: Step, currentTransitive: Seq[Step], targetLhs: T): (Seq[Step], Seq[Step]) = {
        currentBefore match {
          case moreBefore :+ first :+ second if matchTransitiveChaining(first, second, currentStep, outerPath, moreBefore.length).exists(_._1 == targetLhs) =>
            removeWhileTransitive(moreBefore, first, first +: second +: currentTransitive, targetLhs)
          case moreBefore :+ first if matchReplacementChaining(first, currentStep, outerPath, moreBefore.length).exists(_._1 == targetLhs) =>
            removeWhileTransitive(moreBefore, first, first +: currentTransitive, targetLhs)
          case _ =>
            (currentBefore, currentTransitive)
        }
      }
      def withTransitivityInFollowingStep: Option[(Seq[Step], Seq[Step])] = {
        for {
          (firstStep, followingSteps) <- after.headAndTailOption
          nextStep <- followingSteps.headOption
          (moreBefore, previousStep) <- before.initAndLastOption
          (lhs, _, _) <- matchTransitiveChaining(previousStep, firstStep, nextStep, outerPath, before.length - 1)
        } yield removeWhileTransitive(moreBefore, previousStep, Seq(previousStep), lhs)
      }
      def fromCurrentStep: Option[(Seq[Step], Seq[Step])] = {
        for {
          firstStep <- after.headOption
          (_, lhs, _) <- ChainingMethods.getJoiner(firstStep.statement)
        } yield removeWhileTransitive(before, firstStep, Nil, lhs)
      }

      withTransitivityInFollowingStep orElse fromCurrentStep
    }

    splitPrecedingStepsWhileTransitiveGeneric[Statement] orElse splitPrecedingStepsWhileTransitiveGeneric[Term] getOrElse (before, Nil)
  }


  private def matchTransitiveChaining[T <: Expression : ChainingMethods](stepOne: Step, stepTwo: Step, stepThree: Step, outerPath: Seq[Int], firstStepIndex: Int)(implicit stepsWithContext: StepsWithContext): Option[(T, T, T)] = {
    if (GetReferencedLines(stepThree).flatMap(_.asOptionalInstanceOf[StepReference]).map(_.stepPath) == Set(outerPath :+ firstStepIndex, outerPath :+ (firstStepIndex + 1)))
      for {
        (_, lhsOne, rhsOne) <- ChainingMethods.getJoiner(stepOne.statement)
        (_, lhsTwo, rhsTwo) <- ChainingMethods.getJoiner(stepTwo.statement)
        (_, lhsThree, rhsThree) <- ChainingMethods.getJoiner(stepThree.statement)
        if lhsOne == lhsThree && rhsOne == lhsTwo && rhsTwo == rhsThree
      } yield (lhsOne, rhsOne, rhsTwo)
    else None
  }

  private def matchReplacementChaining[T <: Expression : ChainingMethods](stepOne: Step, stepTwo: Step, outerPath: Seq[Int], firstStepIndex: Int)(implicit stepsWithContext: StepsWithContext): Option[(T, T, T)] = {
    if (GetReferencedLines(stepTwo).flatMap(_.asOptionalInstanceOf[StepReference]).map(_.stepPath).contains(outerPath :+ firstStepIndex))
      for {
        (_, lhsOne, rhsOne) <- ChainingMethods.getJoiner(stepOne.statement)
        (_, lhsTwo, rhsTwo) <- ChainingMethods.getJoiner(stepTwo.statement)
        if lhsOne == lhsTwo
      } yield (lhsOne, rhsOne, rhsTwo)
    else None
  }
}
