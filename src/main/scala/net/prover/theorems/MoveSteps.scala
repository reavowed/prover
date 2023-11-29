package net.prover.theorems

import net.prover.controllers.models._
import net.prover.controllers.{BookService, BooleanWithResponseExceptionOps, OptionWithResponseExceptionOps}
import net.prover.entries.StepsWithContext
import net.prover.model._
import net.prover.model.proof.Step
import net.prover.theorems.steps.{InsertExternalBoundVariables, RemoveExternalBoundVariables}
import net.prover.util.FunctorTypes._

import scala.annotation.tailrec
import scala.util.{Success, Try}

object MoveSteps {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepMoveRequest: StepMoveRequest)(
    implicit bookService: BookService
  ): Try[ProofUpdateProps[InsertionAndDeletionProps]] = {
    import stepMoveRequest._
    @tailrec def commonPrefix[T](a: Seq[T], b: Seq[T], acc: Seq[T] = Nil): (Seq[T], Seq[T], Seq[T]) = {
      (a, b) match {
        case (headA +: tailA, headB +: tailB) if headA == headB =>
          commonPrefix(tailA, tailB, acc :+ headA)
        case _ =>
          (acc, a, b)
      }
    }

    val (sharedPath, sourcePathInner, destinationPathInner) = commonPrefix(sourcePath, destinationPath)
    for {
      (proofUpdateProps, insertionAndDeletionProps) <- bookService.replaceSteps[WithValue[InsertionAndDeletionProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, sharedPath) { sharedParentStepsWithContext =>
        for {
          (substepsWithoutCurrent, currentStepsWithContext) <-
            ReplaceSteps[WithValue[StepsWithContext]#Type](sharedParentStepsWithContext, sourcePathInner) { currentStepsWithContext =>
              currentStepsWithContext.steps.splitBetweenIndexesIfValid(sourceStartIndex, sourceEndIndex).map { case (before, steps, after) =>
                (before ++ after, currentStepsWithContext.copy(steps = steps))
              }
            }.orBadRequest("Invalid source path")
          result <- ReplaceSteps[FWithValue[Try, InsertionAndDeletionProps]#Type](sharedParentStepsWithContext.copy(steps = substepsWithoutCurrent), destinationPathInner) { newSurroundingStepsWithContext =>
            val sharedBoundVariableDepth = Seq(currentStepsWithContext.outerStepContext.externalDepth, newSurroundingStepsWithContext.outerStepContext.externalDepth).min
            val boundVariablesToRemove = currentStepsWithContext.outerStepContext.externalDepth - newSurroundingStepsWithContext.outerStepContext.externalDepth
            newSurroundingStepsWithContext.steps.takeAndRemainingIfValid(destinationIndex).map { case (before, after) =>
              for {
                _ <- (0 until sharedBoundVariableDepth).map { i =>
                  (currentStepsWithContext.outerStepContext.boundVariableLists(i).size <= newSurroundingStepsWithContext.outerStepContext.boundVariableLists(i).size).orBadRequest("Cannot move step to one with a smaller bound variable list")
                }.traverseTry
                stepsWithNewContext <- if (boundVariablesToRemove > 0) {
                  RemoveExternalBoundVariables(boundVariablesToRemove)(currentStepsWithContext).orBadRequest("Could not remove extra parameters")
                } else if (boundVariablesToRemove < 0)
                  Success(InsertExternalBoundVariables(-boundVariablesToRemove)(currentStepsWithContext))
                else
                  Success(currentStepsWithContext.steps)
              } yield (
                before ++ stepsWithNewContext ++ after,
                InsertionAndDeletionProps(
                  StepInsertionProps(destinationPath :+ destinationIndex, stepsWithNewContext),
                  StepDeletionProps(sourcePath, sourceStartIndex, sourceEndIndex)))
            }
          }.orBadRequest("Invalid destination path").flatten
        } yield result
      }.orBadRequest(s"Invalid source path")
    } yield proofUpdateProps.withNewStepUpdateProps(
      InsertionAndDeletionProps(
        StepInsertionProps(
          insertionAndDeletionProps.insertion.path,
          destinationPathInner.foldLeft(proofUpdateProps.stepUpdates.newSteps) { (steps, index) => steps(index).asInstanceOf[Step.WithSubsteps].substeps }.slice(destinationIndex, destinationIndex + insertionAndDeletionProps.insertion.newSteps.length)),
        insertionAndDeletionProps.deletion))
  }
}
