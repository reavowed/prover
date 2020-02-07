package net.prover.controllers

import net.prover.controllers.models.{PathData, StepMoveRequest}
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}"))
class ProofController @Autowired() (val bookService: BookService) extends BookModification {

  @PostMapping(value = Array("/{stepPath}/clear"))
  def clearStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)((step, _) =>
      Success(step.provenStatement.map(s => Step.Target(s)).toSeq)
    ).toResponseEntity
  }

  @DeleteMapping(value = Array("/{stepPath}"))
  def deleteStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)((step, _) =>
      // Deleting naming steps is confusing, just clear them
      Success(step.asOptionalInstanceOf[Step.Naming]
        .flatMap(namingStep => namingStep.provenStatement)
        .map(s => Step.Target(s))
        .toSeq)
    ).toResponseEntity
  }

  @PostMapping(value = Array("/moveSteps"))
  def moveStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @RequestBody stepMoveRequest: StepMoveRequest
  ): ResponseEntity[_] = {
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
      result <- bookService.replaceSteps(bookKey, chapterKey, theoremKey, proofIndex, sharedPath) { (sharedParentSteps, sharedContext) =>
        for {
          (substepsWithoutCurrent, (currentSteps, currentStepOuterContext)) <-
            Proof.modifySteps[WithValue[(Seq[Step], StepContext)]#Type](sharedParentSteps, sourcePathInner, sharedContext.stepContext) { (currentSteps, currentStepOuterContext) =>
              currentSteps.splitBetweenIndexesIfValid(sourceStartIndex, sourceEndIndex).map { case (before, steps, after) =>
                (before ++ after, (steps, currentStepOuterContext))
              }
            }.orBadRequest("Invalid source path")
          resultSteps <- Proof.modifySteps(substepsWithoutCurrent, destinationPathInner, sharedContext.stepContext) { (newSurroundingSteps, newStepOuterContext) =>
            val sharedParameterDepth = Seq(currentStepOuterContext.externalDepth, newStepOuterContext.externalDepth).min
            val parametersToRemove = currentStepOuterContext.externalDepth - newStepOuterContext.externalDepth
            val parametersToAdd = -1 * parametersToRemove
            newSurroundingSteps.takeAndRemainingIfValid(destinationIndex).map { case (before, after) =>
              for {
                _ <- (0 until sharedParameterDepth).map { i =>
                  (currentStepOuterContext.boundVariableLists(i).size <= newStepOuterContext.boundVariableLists(i).size).orBadRequest("Cannot move step to one with a smaller bound variable list")
                }.traverseTry
                stepsWithNewContext <- if (parametersToRemove > 0)
                  currentSteps.map(_.removeExternalParameters(parametersToRemove, 0)).traverseOption.orBadRequest("Could not remove extra parameters")
                else if (parametersToAdd > 0)
                  Success(currentSteps.map(_.insertExternalParameters(parametersToAdd, 0)))
                else
                  Success(currentSteps)
              } yield before ++ stepsWithNewContext ++ after
            }
          }.orBadRequest("Invalid destination path").flatten
        } yield resultSteps
      }.orBadRequest(s"Invalid source path")
    } yield result).toResponseEntity
  }
}
