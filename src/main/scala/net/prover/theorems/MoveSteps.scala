package net.prover.theorems

import net.prover.controllers.models.{InsertionAndDeletionProps, ProofUpdateProps, StepDeletionProps, StepInsertionProps, StepMoveRequest}
import net.prover.controllers.{BookService, BooleanWithResponseExceptionOps, OptionWithResponseExceptionOps}
import net.prover.model._
import net.prover.model.proof.{Step, StepContext}
import net.prover.theorems.steps.{InsertExternalBoundVariables, RemoveExternalBoundVariables}
import net.prover.util.FunctorTypes._

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
    def commonPrefix[T](a: Seq[T], b: Seq[T], acc: Seq[T] = Nil): (Seq[T], Seq[T], Seq[T]) = {
      (a, b) match {
        case (headA +: tailA, headB +: tailB) if headA == headB =>
          commonPrefix(tailA, tailB, acc :+ headA)
        case _ =>
          (acc, a, b)
      }
    }

    val (sharedPath, sourcePathInner, destinationPathInner) = commonPrefix(sourcePath, destinationPath)
    (for {
      (proofUpdateProps, insertionAndDeletionProps) <- bookService.replaceSteps[WithValue[InsertionAndDeletionProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, sharedPath) { (sharedParentSteps, sharedContext) =>
        for {
          (substepsWithoutCurrent, (currentSteps, currentStepOuterContext)) <-
            ReplaceSteps[WithValue[(Seq[Step], StepContext)]#Type](sharedParentSteps, sourcePathInner, sharedContext.stepContext) { (currentSteps, currentStepOuterContext) =>
              currentSteps.splitBetweenIndexesIfValid(sourceStartIndex, sourceEndIndex).map { case (before, steps, after) =>
                (before ++ after, (steps, currentStepOuterContext))
              }
            }.orBadRequest("Invalid source path")
          result <- ReplaceSteps[TryWithValue[InsertionAndDeletionProps]#Type](substepsWithoutCurrent, destinationPathInner, sharedContext.stepContext) { (newSurroundingSteps, newStepOuterContext) =>
            val sharedBoundVariableDepth = Seq(currentStepOuterContext.externalDepth, newStepOuterContext.externalDepth).min
            val boundVariablesToRemove = currentStepOuterContext.externalDepth - newStepOuterContext.externalDepth
            newSurroundingSteps.takeAndRemainingIfValid(destinationIndex).map { case (before, after) =>
              for {
                _ <- (0 until sharedBoundVariableDepth).map { i =>
                  (currentStepOuterContext.boundVariableLists(i).size <= newStepOuterContext.boundVariableLists(i).size).orBadRequest("Cannot move step to one with a smaller bound variable list")
                }.traverseTry
                stepsWithNewContext <- if (boundVariablesToRemove > 0) {
                  RemoveExternalBoundVariables(currentSteps, currentStepOuterContext, boundVariablesToRemove).orBadRequest("Could not remove extra parameters")
                } else if (boundVariablesToRemove < 0)
                  Success(InsertExternalBoundVariables(currentSteps, currentStepOuterContext, -boundVariablesToRemove))
                else
                  Success(currentSteps)
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
        insertionAndDeletionProps.deletion)))
  }
}
