package net.prover.controllers

import net.prover.controllers.models.{PathData, StepMoveRequest}
import net.prover.model._
import net.prover.model.proof._
import net.prover.theorems.MoveSteps
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/proofs/{proofIndex}"))
class ProofController @Autowired() (implicit val bookService: BookService) {

  @PostMapping(value = Array("/{stepPath}/clear"))
  def clearStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    bookService.replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)(stepWithContext =>
      Success(Seq(Step.Target(stepWithContext.step.statement)))
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
    bookService.replaceStep[Step.WithSubsteps](bookKey, chapterKey, theoremKey, proofIndex, stepPath)(stepWithContext =>
      Success(stepWithContext.step.substeps)
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
    bookService.replaceStep[Step](bookKey, chapterKey, theoremKey, proofIndex, stepPath)(stepWithContext =>
      // Deleting naming steps is confusing, just clear them
      Success(stepWithContext.step.asOptionalInstanceOf[Step.Naming]
        .map(namingStep => Step.Target(namingStep.statement))
        .toSeq)
    ).toResponseEntity
  }

  @PostMapping(value = Array("/moveSteps"))
  def moveSteps(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("proofIndex") proofIndex: Int,
    @RequestBody stepMoveRequest: StepMoveRequest
  ): ResponseEntity[_] = {
    MoveSteps(bookKey, chapterKey, theoremKey, proofIndex, stepMoveRequest).toResponseEntity
  }
}
