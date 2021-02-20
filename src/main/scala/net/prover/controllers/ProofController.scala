package net.prover.controllers

import net.prover._
import net.prover.controllers.models._
import net.prover.model.proof._
import net.prover.structure.BookService
import net.prover.structure.model.entries.Theorem.Proof
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

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

  @PostMapping(value = Array("/{stepPath}/unpack"))
  def unpackStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step.WithSubsteps](bookKey, chapterKey, theoremKey, proofIndex, stepPath)((step, _) =>
      Success(step.substeps)
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
      (proofUpdateProps, insertionAndDeletionProps) <- bookService.replaceSteps[WithValue[InsertionAndDeletionProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, sharedPath) { (sharedParentSteps, sharedContext) =>
        for {
          (substepsWithoutCurrent, (currentSteps, currentStepOuterContext)) <-
            Proof.modifySteps[WithValue[(Seq[Step], StepContext)]#Type](sharedParentSteps, sourcePathInner, sharedContext.stepContext) { (currentSteps, currentStepOuterContext) =>
              currentSteps.splitBetweenIndexesIfValid(sourceStartIndex, sourceEndIndex).map { case (before, steps, after) =>
                (before ++ after, (steps, currentStepOuterContext))
              }
            }.orBadRequest("Invalid source path")
          result <- Proof.modifySteps[TryWithValue[InsertionAndDeletionProps]#Type](substepsWithoutCurrent, destinationPathInner, sharedContext.stepContext) { (newSurroundingSteps, newStepOuterContext) =>
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
              } yield (before ++ stepsWithNewContext ++ after, InsertionAndDeletionProps(StepInsertionProps(destinationPath :+ destinationIndex, stepsWithNewContext), StepDeletionProps(sourcePath, sourceStartIndex, sourceEndIndex)))
            }
          }.orBadRequest("Invalid destination path").flatten
        } yield result
      }.orBadRequest(s"Invalid source path")
    } yield proofUpdateProps.withNewStepUpdateProps(
      InsertionAndDeletionProps(
        StepInsertionProps(
          insertionAndDeletionProps.insertion.path,
          destinationPathInner.foldLeft(proofUpdateProps.stepUpdates.newSteps) { (steps, index) => steps(index).asInstanceOf[Step.WithSubsteps].substeps }.slice(destinationIndex, destinationIndex + insertionAndDeletionProps.insertion.newSteps.length)),
        insertionAndDeletionProps.deletion))).toResponseEntity
  }
}
